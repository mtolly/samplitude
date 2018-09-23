{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad                    (forM, forM_, guard, void,
                                                   when)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Resource     (MonadResource, runResourceT)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum)
import           Data.Conduit                     ((.|))
import qualified Data.Conduit                     as C
import qualified Data.Conduit.Audio               as A
import qualified Data.Conduit.Audio.SampleRate    as SR
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Int
import           Data.List                        (isPrefixOf, nub)
import           Data.List                        (isInfixOf)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Vector.Storable             as V
import qualified Data.Vector.Storable.Mutable     as MV
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Message.Channel       as EC
import qualified Sound.MIDI.Message.Channel.Voice as ECV
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (createDirectoryIfMissing,
                                                   listDirectory)
import           System.Environment               (getArgs)
import           System.FilePath                  (dropExtension, takeDirectory,
                                                   takeExtension, takeFileName,
                                                   (-<.>), (<.>), (</>))

import           Amplitude
import           Audio
import           Frequency
import           Reaper

renderSamples
  :: (MonadResource m, MonadIO n)
  => RTB.T U.Seconds (Int, Int, Int, SDESEntry, V.Vector Int16, U.Seconds)
  -> m (A.AudioSource n Int16)
renderSamples rtb = do
  let samps = ATB.toPairList $ RTB.toAbsoluteEventList 0 rtb
      outputRate = 48000 :: Double
      lengthSecs = foldr max 0
        [ secs + samplen
        | (secs, (_, _, _, _, _, samplen)) <- samps
        ]
  mv <- liftIO $ MV.new $ floor $ 2 * realToFrac (lengthSecs + 1) * outputRate
  liftIO $ MV.set mv 0
  forM_ samps $ \(secs, (inputRate, notePitch, noteVel, sdes, v, len)) -> do
    let transposeFreq = 2 ** (fromIntegral (sdesTranspose sdes + (notePitch - sdesBasePitch sdes)) / 12)
        claimedRate = realToFrac inputRate * transposeFreq
        src
          = A.takeStart (A.Seconds $ realToFrac len)
          $ SR.resampleTo outputRate SR.SincMediumQuality
          $ A.AudioSource (C.yield appliedPanVol) claimedRate 2 $ V.length v
        appliedPanVol = let
          v' = V.map A.fractionalSample v
          in V.generate (V.length v * 2) $ \i -> case quotRem i 2 of
            (j, 0) -> (v' V.! j) * volRatio * panLeftRatio
            (j, _) -> (v' V.! j) * volRatio * panRightRatio
        volRatio = fromIntegral noteVel / 0x7F
        panNormal = (fromIntegral (sdesPan sdes) / 0x7F) * 2 - 1
        theta = panNormal * (pi / 4)
        panLeftRatio  = (sqrt 2 / 2) * (cos theta - sin theta)
        panRightRatio = (sqrt 2 / 2) * (cos theta + sin theta)
        writeToPosn framePosn = C.await >>= \case
          Nothing -> return ()
          Just v' -> do
            let samplePosn = framePosn * 2
            currentArea <- liftIO $ V.freeze $ MV.slice samplePosn (V.length v') mv
            let mixed = V.zipWith (+) currentArea v'
            liftIO $ V.copy (MV.slice samplePosn (V.length mixed) mv) mixed
            writeToPosn $ samplePosn + V.length v'
    C.runConduit $ A.source src .| writeToPosn (floor $ realToFrac secs * outputRate)
  v <- liftIO $ V.unsafeFreeze mv
  return $ A.mapSamples A.integralSample $ A.AudioSource (C.yield v) outputRate 2 $ V.length v `quot` 2

trackState :: (NNC.C t) => s -> (s -> t -> a -> (s, Maybe b)) -> RTB.T t a -> RTB.T t b
trackState curState step rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case step curState dt x of
    (nextState, Nothing) -> RTB.delay dt   $ trackState nextState step rtb'
    (nextState, Just y ) -> RTB.cons  dt y $ trackState nextState step rtb'

applyStatus1 :: (NNC.C t, Ord s, Ord a) => s -> RTB.T t s -> RTB.T t a -> RTB.T t (s, a)
applyStatus1 start status events = let
  fn current _ = \case
    Left  s -> (s      , Nothing          )
    Right x -> (current, Just (current, x))
  in trackState start fn $ RTB.merge (fmap Left status) (fmap Right events)

-- | Returns triples of (pitch, velocity, length)
getMIDINotes :: (NNC.C t) => RTB.T t E.T -> RTB.T t (Int, Int, t)
getMIDINotes trk = let
  edges = flip RTB.mapMaybe trk $ \case
    E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.NoteOn p v))) -> let
      v' = ECV.fromVelocity v
      in Just (ECV.fromPitch p, guard (v' /= 0) >> Just v')
    E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.NoteOff p _)))
      -> Just (ECV.fromPitch p, Nothing)
    _ -> Nothing
  offFor p (p', Nothing) = guard (p == p') >> Just ()
  offFor _ _             = Nothing
  go es = case RTB.viewL es of
    Nothing -> RTB.empty
    Just ((dt, (p, Just v)), es') -> case U.extractFirst (offFor p) es' of
      Nothing                -> RTB.delay dt $ go es' -- on with no matching off
      Just ((len, ()), es'') -> RTB.cons dt (p, v, len) $ go es''
    Just ((dt, (_, Nothing)), es') -> RTB.delay dt $ go es' -- off with no matching on
  in go $ RTB.normalize edges -- we just need (p, Nothing) before (p, Just _)

rtbPartitionLists :: (NNC.C t) => RTB.T t ([a], [b]) -> (RTB.T t a, RTB.T t b)
rtbPartitionLists trk = (RTB.flatten $ fmap fst trk, RTB.flatten $ fmap snd trk)

data SongAmplitude = SongAmplitude
  { ampMidi :: FilePath
  , ampBnks :: [FilePath]
  } deriving (Eq, Show)

data SongFrequency = SongFrequency
  { freqPy   :: FilePath
  , freqMidi :: FilePath
  , freqHds  :: [FilePath]
  } deriving (Eq, Show)

findSong :: FilePath -> IO (Either SongAmplitude SongFrequency)
findSong dir = do
  files <- listDirectory dir
  let findMidi sfx = case filter ((reverse sfx `isPrefixOf`) . reverse) files of
        mid : _ -> return mid
        []      -> error $ "Couldn't find MIDI file with suffix " ++ show sfx
  case filter ((== ".py") . takeExtension) files of
    py : _ -> do
      mid <- findMidi "_c.mid"
      let bankPrefix = dropExtension py
          findHDs n = let
            lookFor = (bankPrefix ++ "_" ++ show n ++ ".hd")
              : [bankPrefix <.> "hd" | n == 1]
            in case filter (`elem` lookFor) files of
              hd : _ -> (hd :) <$> findHDs (n + 1)
              []     -> return []
      hds <- findHDs (1 :: Int)
      return $ Right $ SongFrequency (dir </> py) (dir </> mid) (map (dir </>) hds)
    [] -> do
      mid <- findMidi "_g.mid"
      let banks = filter ((== ".bnk") . takeExtension) files
      return $ Left $ SongAmplitude (dir </> mid) (map (dir </>) banks)

main :: IO ()
main = getArgs >>= \case

  [] -> do
    putStrLn "samplitude (Amplitude [PS2] audio renderer)"
    putStrLn "Drag a song folder onto this .exe to run."
    putStrLn "(press enter to close)"
    void getLine

  "print" : bnkPaths -> forM_ bnkPaths $ \bnkPath -> case takeExtension bnkPath of
    ".hd" -> do
      hd <- runGet getHD . BL.fromStrict <$> B.readFile bnkPath
      let printList typ xs = do
            putStrLn $ "LIST: " <> typ
            putStrLn ""
            forM_ (zip [0..] xs) $ \(i, x) -> do
              putStrLn $ typ <> " #" <> show (i :: Int)
              print x
              putStrLn ""
      printList "Prog" $ hdProg hd
      printList "Sset" $ hdSset hd
      printList "Smpl" $ hdSmpl hd
      printList "Vagi" $ hdVagi hd
    ".bnk" -> do
      putStrLn bnkPath
      putStrLn ""
      bnk <- BL.fromStrict <$> B.readFile bnkPath
      let chunks = runGet bnkChunks bnk
      putStrLn "INSTRUMENTS"
      forM_ (zip (concat [xs | INST xs <- chunks]) (concat [xs | INNM xs <- chunks])) $ \(ent, name) -> do
        print name
        print ent
      putStrLn ""
      putStrLn "SAMPLE DIRECTIVES"
      forM_ (zip (concat [xs | SDES xs <- chunks]) (concat [xs | SDNM xs <- chunks])) $ \(ent, name) -> do
        print name
        print ent
      putStrLn ""
      putStrLn "SAMPLES"
      forM_ (zip (concat [xs | SAMP xs <- chunks]) (concat [xs | SANM xs <- chunks])) $ \(ent, name) -> do
        print name
        print ent
      putStrLn ""
    _ -> error $ "Unrecognized bank metadata file: " ++ bnkPath

  "samples" : bnks -> forM_ bnks $ \bnkPath -> case takeExtension bnkPath of
    ".hd" -> do
      hd <- BL.fromStrict <$> B.readFile bnkPath
      bd <- BL.fromStrict <$> B.readFile (bnkPath -<.> "bd")
      let outDir = dropExtension bnkPath ++ "_samples"
      createDirectoryIfMissing False outDir
      let chunks = runGet getHD hd
      forM_ (zip [0..] $ hdVagi chunks) $ \(i, mentry) -> forM_ mentry $ \entry -> do
        let bytes = BL.drop (fromIntegral $ vagiFilePosition entry) bd
            samples = V.fromList $ decodeSamples bytes
        print (i, entry)
        runResourceT $ writeWAV (outDir </> show (i :: Int) <.> "wav") $ A.AudioSource
          (C.yield samples)
          (realToFrac $ vagiRate entry)
          1
          (V.length samples)
    ".bnk" -> do
      bnk <- BL.fromStrict <$> B.readFile bnkPath
      nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
      let outDir = dropExtension bnkPath ++ "_samples"
      createDirectoryIfMissing False outDir
      let chunks = runGet bnkChunks bnk
          samp = concat [ xs | SAMP xs <- chunks ]
          sanm = concat [ xs | SANM xs <- chunks ]
      forM_ (zip samp sanm) $ \(entry, name) -> do
        let bytes = BL.drop (fromIntegral $ sampFilePosition entry) nse
            samples = V.fromList $ decodeSamples bytes
        when (sampRate entry /= 0) $ do
          print (entry, name)
          runResourceT $ writeWAV (outDir </> B8.unpack name <.> "wav") $ A.AudioSource
            (C.yield samples)
            (realToFrac $ sampRate entry)
            1
            (V.length samples)
    _ -> error $ "Unrecognized bank metadata file: " ++ bnkPath

  "reaper" : songDirs -> forM_ songDirs $ \songDir -> do
    putStrLn $ "## " ++ songDir
    findSong songDir >>= \case
      Right SongFrequency{} -> return () -- not implemented
      Left (SongAmplitude midPath bnkPaths) -> do
        Left trks <- U.decodeFile <$> Load.fromFile midPath
        sounds <- forM bnkPaths $ \bnkPath -> do
          bnk <- BL.fromStrict <$> B.readFile bnkPath
          nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
          let outDir = dropExtension bnkPath ++ "_samples"
              outDirRelative = takeFileName outDir
          createDirectoryIfMissing False outDir
          let chunks = runGet bnkChunks bnk
              samp = concat [ xs | SAMP xs <- chunks ]
              sanm = concat [ xs | SANM xs <- chunks ]
          i <- case [ b | BANK b <- chunks ] of
            []    -> error $ "Bank " ++ show bnkPath ++ " has no BANK chunk"
            b : _ -> return $ bankNumber b
          samps <- forM (zip samp sanm) $ \(entry, name) -> do
            let bytes = BL.drop (fromIntegral $ sampFilePosition entry) nse
                samples = V.fromList $ decodeSamples bytes
                sampPath         = outDir         </> B8.unpack name <.> "wav"
                sampPathRelative = outDirRelative </> B8.unpack name <.> "wav"
            if sampRate entry /= 0
              then do
                runResourceT $ writeWAV sampPath $ A.AudioSource
                  (C.yield samples)
                  (fromIntegral $ sampRate entry)
                  1
                  (V.length samples)
                return $ Just (sampPathRelative, fromIntegral (V.length samples) / fromIntegral (sampRate entry))
              else return Nothing
          return (i, (chunks, samps))
        let outRPP = songDir </> "project.RPP"
        (writeRPP outRPP =<<) $ rpp "REAPER_PROJECT" ["0.1", "5.0/OSX64", "1449358215"] $ do
          line "VZOOMEX" ["0"]
          line "ITEMMIX" ["1"] -- always mix stacked audio items
          line "SAMPLERATE" ["44100", "0", "0"]
          block "METRONOME" ["6", "2"] $ return () -- disables metronome
          let tmap = U.makeTempoMap $ head trks
          tempoTrack $ RTB.toAbsoluteEventList 0 $ U.applyTempoTrack tmap $ processTempoTrack $ head trks
          forM_ (zip [0..] $ tail trks) $ \(i, trkBeats) -> let
            trk = U.applyTempoTrack tmap trkBeats
            soundNotes = RTB.filter (\(p, _, _) -> p < 96) $ getMIDINotes trk
            bankChanges = flip RTB.mapMaybe trk $ \case
              E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.Control cont v)))
                | ECV.fromController cont == 0
                -> Just v
              _ -> Nothing
            progChanges = flip RTB.mapMaybe trk $ \case
              E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.ProgramChange prog)))
                -> Just $ ECV.fromProgram prog
              _ -> Nothing
            applied
              = applyStatus1 Nothing (fmap Just bankChanges)
              $ applyStatus1 Nothing (fmap Just progChanges)
              $ soundNotes
            (warnings, appliedSources) = rtbPartitionLists $ flip fmap applied $ \case
              (Just bank, (Just prog, (pitch, vel, len))) -> case lookup bank sounds of
                Nothing -> (["No bank with index " ++ show bank], [])
                Just (chunks, samps) -> let
                  insts = concat [ xs | INST xs <- chunks ]
                  in case break ((== prog) . instProgNumber) insts of
                    (_, []) -> (["Bank " ++ show bank ++ " doesn't have instrument with prog " ++ show prog], [])
                    (skipInsts, inst : _) -> let
                      skipSDES = sum $ map instSDESCount skipInsts
                      sdeses = take (instSDESCount inst) $ drop skipSDES $ concat [ xs | SDES xs <- chunks ]
                      in case [ sd | sd <- sdeses, sdesMinPitch sd <= pitch && pitch <= sdesMaxPitch sd ] of
                        [] -> ([unwords
                          [ "No SDES entry found for bank", show bank
                          , "program", show prog
                          , "pitch", show pitch
                          ]], [])
                        -- note: there can be more than 1 matching SDES. e.g. stereo pairs of samples in supersprode
                        sdesMatch -> mconcat $ flip map sdesMatch $ \sdes -> case drop (sdesSAMPNumber sdes) samps of
                          Just (path, audioLen) : _ -> ([], [(path, sdes, pitch, vel, min len audioLen)])
                          Nothing : _ -> (["Bank " ++ show bank ++ " has empty sample at index " ++ show (sdesSAMPNumber sdes)], [])
                          _ -> (["Bank " ++ show bank ++ " doesn't have sample index " ++ show (sdesSAMPNumber sdes)], [])
              _ -> (["Notes before bank and prog have been set"], [])
            in do
              liftIO $ putStrLn $ "# Track " ++ show (i :: Int) ++ ": " ++ maybe "no name" show (U.trackName trk)
              liftIO $ forM_ (nub $ RTB.getBodies warnings) putStrLn
              block "TRACK" [] $ do
                let name = fromMaybe "Unnamed track" $ U.trackName trk
                line "NAME" [name]
                let encodeColor (r, g, b) = 0x1000000 + 0x10000 * b + 0x100 * g + r :: Int
                    color rgb = line "PEAKCOL" [show $ encodeColor rgb]
                if  | "CATCH:D:" `isInfixOf` name -> color (244, 75, 75)  -- red
                    | "CATCH:B:" `isInfixOf` name -> color (78, 62, 224)  -- blue
                    | "CATCH:S:" `isInfixOf` name -> color (232, 221, 76) -- yellow
                    | "CATCH:V:" `isInfixOf` name -> color (95, 232, 88)  -- green
                    | "CATCH:G:" `isInfixOf` name -> color (237, 171, 56) -- orange
                    | "CATCH:F:" `isInfixOf` name -> color (233, 62, 242) -- purple
                    | otherwise              -> return ()
                line "MUTESOLO"
                  [ if   "AXE_CONTOUR" `isInfixOf` name
                      || "AXE_HARMONY" `isInfixOf` name
                      || "SCRATCH"     `isInfixOf` name
                    then "1" -- muted
                    else "0" -- not muted
                  , "0" -- 2 if solo'd, else 0
                  , "0" -- dunno
                  ]
                line "TRACKHEIGHT" ["0", "0"]
                line "FX" ["0"]
                block "FXCHAIN" [] $ return ()
                forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 appliedSources) $ \(posn, (path, sdes, pitch, vel, len)) -> do
                  insertAudio posn len
                    (sdesTranspose sdes + (pitch - sdesBasePitch sdes))
                    ((fromIntegral (sdesPan sdes) / 0x7F) * 2 - 1)
                    (fromIntegral vel / 0x7F)
                    path

  songDirs -> forM_ songDirs $ \songDir -> do
    putStrLn $ "## " ++ songDir
    findSong songDir >>= \case
      Left (SongAmplitude midPath bnkPaths) -> do
        Left trks <- U.decodeFile <$> Load.fromFile midPath
        sounds <- forM bnkPaths $ \bnkPath -> do
          bnk <- BL.fromStrict <$> B.readFile bnkPath
          nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
          let chunks = runGet bnkChunks bnk
          i <- case [ b | BANK b <- chunks ] of
            []    -> error $ "Bank " ++ show bnkPath ++ " has no BANK chunk"
            b : _ -> return $ bankNumber b
          return (i, (chunks, nse))
        let tmap = U.makeTempoMap $ head trks
        forM_ (zip [0..] $ tail trks) $ \(i, trkBeats) -> let
          trk = U.applyTempoTrack tmap trkBeats
          soundNotes = RTB.filter (\(p, _, _) -> p < 96) $ getMIDINotes trk
          bankChanges = flip RTB.mapMaybe trk $ \case
            E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.Control cont v)))
              | ECV.fromController cont == 0
              -> Just v
            _ -> Nothing
          progChanges = flip RTB.mapMaybe trk $ \case
            E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.ProgramChange prog)))
              -> Just $ ECV.fromProgram prog
            _ -> Nothing
          applied
            = applyStatus1 Nothing (fmap Just bankChanges)
            $ applyStatus1 Nothing (fmap Just progChanges)
            $ soundNotes
          (warnings, appliedSources) = rtbPartitionLists $ flip fmap applied $ \case
            (Just bank, (Just prog, (pitch, vel, len))) -> case lookup bank sounds of
              Nothing -> (["No bank with index " ++ show bank], [])
              Just (chunks, nse) -> let
                insts = concat [ xs | INST xs <- chunks ]
                in case break ((== prog) . instProgNumber) insts of
                  (_, []) -> (["Bank " ++ show bank ++ " doesn't have instrument with prog " ++ show prog], [])
                  (skipInsts, inst : _) -> let
                    skipSDES = sum $ map instSDESCount skipInsts
                    sdeses = take (instSDESCount inst) $ drop skipSDES $ concat [ xs | SDES xs <- chunks ]
                    in case [ sd | sd <- sdeses, sdesMinPitch sd <= pitch && pitch <= sdesMaxPitch sd ] of
                      [] -> ([unwords
                        [ "No SDES entry found for bank", show bank
                        , "program", show prog
                        , "pitch", show pitch
                        ]], [])
                      -- note: there can be more than 1 matching SDES. e.g. stereo pairs of samples in supersprode
                      sdesMatch -> mconcat $ flip map sdesMatch $ \sdes -> case drop (sdesSAMPNumber sdes) $ concat [ xs | SAMP xs <- chunks ] of
                        [] -> (["Bank " ++ show bank ++ " doesn't have sample index " ++ show (sdesSAMPNumber sdes)], [])
                        samp : _ -> let
                          bytes = BL.drop (fromIntegral $ sampFilePosition samp) nse
                          samples = V.fromList $ decodeSamples bytes
                          in ([], [(sampRate samp, pitch, vel, sdes, samples, len + 0.001)])
            _ -> (["Notes before bank and prog have been set"], [])
          in do
            putStrLn $ "# Track " ++ show i ++ ": " ++ maybe "no name" show (U.trackName trk)
            forM_ (nub $ RTB.getBodies warnings) putStrLn
            let name = songDir </> concat
                  [ if (i :: Int) < 10 then '0' : show i else show i
                  , "_"
                  , map (\c -> if isAlphaNum c then c else '_') $ fromMaybe "" $ U.trackName trk
                  , ".wav"
                  ]
            if RTB.null appliedSources
              then putStrLn "No audio to render"
              else do
                audio <- runResourceT $ renderSamples appliedSources
                runResourceT $ writeWAV name audio
      Right (SongFrequency pyPath midPath hdPaths) -> do
        sects <- getBankSections . B8.unpack <$> B.readFile pyPath
        Left trks <- U.decodeFile <$> Load.fromFile midPath
        sounds <- forM (zip [0..] hdPaths) $ \(i, hdPath) -> do
          hd <- BL.fromStrict <$> B.readFile hdPath
          bd <- BL.fromStrict <$> B.readFile (hdPath -<.> "bd")
          let chunks = runGet getHD hd
          return (i, (chunks, bd))
        let tmap = U.makeTempoMap $ head trks
            bankChanges
              = U.applyTempoTrack tmap
              $ RTB.fromAbsoluteEventList
              $ ATB.fromPairList
              $ flip zip ([0..] :: [Int])
              $ case sects of
                Nothing   -> [0]
                Just bars -> 0 : [ fromIntegral $ s * 4 | s <- bars ]
        forM_ (zip [0..] $ tail trks) $ \(i, trkBeats) -> let
          trk = U.applyTempoTrack tmap trkBeats
          soundNotes = RTB.filter (\(p, _, _) -> p < 96) $ getMIDINotes trk
          progChanges = flip RTB.mapMaybe trk $ \case
            E.MIDIEvent (EC.Cons _ (EC.Voice (ECV.ProgramChange prog)))
              -> Just $ ECV.fromProgram prog
            _ -> Nothing
          applied
            = applyStatus1 Nothing (fmap Just bankChanges)
            $ applyStatus1 Nothing (fmap Just progChanges)
            $ soundNotes
          (warnings, appliedSources) = rtbPartitionLists $ flip fmap applied $ \case
            (Just bank, (Just prog, (pitch, _vel, len))) -> case lookup bank sounds of
              Nothing -> (["No bank with index " ++ show bank], [])
              Just (hd, bd) -> case drop prog $ hdProg hd of
                Just progEntry : _ -> case [ row | row <- progRows progEntry, progRowMinPitch row <= pitch && pitch <= progRowMaxPitch row ] of
                  [] -> ([unwords
                    [ "No Prog row found for bank", show bank
                    , "program", show prog
                    , "pitch", show pitch
                    ]], [])
                  rows -> mconcat $ flip map rows $ \row -> case drop (progRowSsetIndex row) $ hdSset hd of
                    Just sset : _ -> case drop (ssetSmplIndex sset) $ hdSmpl hd of
                      Just smpl : _ -> case drop (smplVagiIndex smpl) $ hdVagi hd of
                        Just vagi : _ -> let
                          bytes = BL.drop (fromIntegral $ vagiFilePosition vagi) bd
                          samples = V.fromList $ decodeSamples bytes
                          sdes = SDESEntry
                            { sdesMinPitch   = progRowMinPitch row
                            , sdesMaxPitch   = progRowMaxPitch row
                            , sdesBasePitch  = smplBasePitch smpl
                            , sdesTranspose  = 0
                            , sdesPan        = progRowPan row
                            , sdesSAMPNumber = undefined
                            , sdesBytes      = undefined
                            }
                          -- this is a complete guess. some samples cut off at note end and some don't
                          len' = if smplMaybeCutoff smpl >= 0xd0
                            then 10
                            else len + 0.001
                          vol = 0x7F -- progRowVol row (TODO figure this out)
                          in ([], [(vagiRate vagi, pitch, vol, sdes, samples, len')])
                        _ -> (["no vagi"], [])
                      _ -> (["no smpl"], [])
                    _ -> (["no sset"], [])
                _ -> (["Bank " ++ show bank ++ " doesn't have a Prog index " ++ show prog], [])
            _ -> (["Notes before bank and prog have been set"], [])
          in do
            putStrLn $ "# Track " ++ show i ++ ": " ++ maybe "no name" show (U.trackName trk)
            forM_ (nub $ RTB.getBodies warnings) putStrLn
            let name = takeDirectory pyPath </> concat
                  [ if (i :: Int) < 10 then '0' : show i else show i
                  , "_"
                  , map (\c -> if isAlphaNum c then c else '_') $ fromMaybe "" $ U.trackName trk
                  , ".wav"
                  ]
            if RTB.null appliedSources
              then putStrLn "No audio to render"
              else do
                audio <- runResourceT $ renderSamples appliedSources
                runResourceT $ writeWAV name audio

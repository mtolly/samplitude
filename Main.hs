{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad                    (forM, forM_, guard,
                                                   replicateM, when)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Resource     (MonadResource, runResourceT)
import qualified Control.Monad.Trans.State        as S
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum)
import           Data.Conduit                     ((.|))
import qualified Data.Conduit                     as C
import qualified Data.Conduit.Audio               as A
import qualified Data.Conduit.Audio.SampleRate    as SR
import qualified Data.Conduit.List                as CL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Int
import           Data.List                        (intercalate, nub,
                                                   stripPrefix)
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Vector.Storable             as V
import qualified Data.Vector.Storable.Mutable     as MV
import           Data.Word
import           GHC.IO.Handle                    (HandlePosn (..))
import           Numeric
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Message.Channel       as EC
import qualified Sound.MIDI.Message.Channel.Voice as ECV
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (createDirectoryIfMissing,
                                                   listDirectory)
import           System.Environment               (getArgs)
import           System.FilePath                  (dropExtension, takeExtension,
                                                   (-<.>), (<.>), (</>))
import qualified System.IO                        as IO

data SAMPEntry = SAMPEntry
  { sampChannels     :: Int -- guessing
  , sampRate         :: Int
  , sampFilePosition :: Int
  } deriving (Eq, Show)

data SDESEntry = SDESEntry
  { sdesMinPitch   :: Int
  , sdesMaxPitch   :: Int
  , sdesBasePitch  :: Int
  , sdesTranspose  :: Int -- semitones to move. seen in supersprode bass
  , sdesPan        :: Int -- 0x00 is left, 0x40 is center, 0x7F is right
  , sdesSAMPNumber :: Int
  -- lots of other parts not deciphered yet
  , sdesBytes      :: B.ByteString
  } deriving (Eq, Show)

data INSTEntry = INSTEntry
  { instProgNumber :: Int
  , instSDESCount  :: Int
  -- other parts not deciphered yet
  , instBytes      :: B.ByteString
  } deriving (Eq, Show)

data BANKEntry = BANKEntry
  { bankNumber :: Int
  , bankINSTCount :: Int
  -- other parts not deciphered yet
  , bankBytes  :: B.ByteString
  } deriving (Eq, Show)

data Chunk
  = SAMP [SAMPEntry] -- samples
  | SANM [B.ByteString] -- sample names
  | SAFN [B.ByteString] -- sample filenames
  | BANK BANKEntry -- bank(s?)
  | BKNM B.ByteString -- bank name
  | INST [INSTEntry] -- instruments
  | INNM [B.ByteString] -- instrument names
  | SDES [SDESEntry] -- sample... directives?
  | SDNM [B.ByteString] -- SDES names
  deriving (Eq, Show)

sizedArea :: (B.ByteString -> Get a) -> Get a
sizedArea g = do
  size <- getWord32le
  bs <- lookAhead $ getByteString $ fromIntegral size
  startPosn <- bytesRead
  x <- g bs
  endPosn <- bytesRead
  if startPosn + fromIntegral size == endPosn
    then return x
    else fail $ unwords
      [ "Struct with size at", show startPosn
      , "should've been", show size
      , "bytes but was", show (endPosn - startPosn)
      ]

getSAMPEntry :: Get SAMPEntry
getSAMPEntry = sizedArea $ \_ -> do
  chans <- getWord32le -- guessing this is channels, all 1 in files i've seen
  rate <- getWord32le
  0 <- getWord32le -- dunno what this space is for
  0 <- getWord16le
  posn <- getWord32le
  return $ SAMPEntry (fromIntegral chans) (fromIntegral rate) (fromIntegral posn)

getSDESEntry :: Get SDESEntry
getSDESEntry = sizedArea $ \bs -> do
  endbytes <- getWord32le -- size of some unknown variable field at end
  minPitch <- getWord8
  maxPitch <- getWord8
  basePitch <- getWord8
  transpose <- getInt8 -- usually 0. used in supersprode bass
  _ <- getByteString 12 -- lots of unknown stuff
  _vol <- getWord8 -- I previously thought this was volume but don't think so now
  pan <- getWord8
  samp <- getWord8
  _ <- getByteString $ 3 + fromIntegral endbytes -- all 0
  return $ SDESEntry
    (fromIntegral minPitch)
    (fromIntegral maxPitch)
    (fromIntegral basePitch)
    (fromIntegral transpose)
    (fromIntegral pan)
    (fromIntegral samp)
    bs

getINSTEntry :: Get INSTEntry
getINSTEntry = sizedArea $ \bs -> do
  _ <- getWord32le -- observed 1
  prog <- getWord16le
  _ <- getWord16le -- unknown
  _ <- getWord16le -- observed 0
  sdes <- getWord16le -- number of SDES entries for this instrument
  return $ INSTEntry (fromIntegral prog) (fromIntegral sdes) bs

getBANK :: Word32 -> Get BANKEntry
getBANK n = do
  bs <- getByteString $ fromIntegral n
  let bankNum = B.index bs 8
      instCount = B.index bs 11
  -- I assume you'd use the inst count with multiple BANK entries in a .bnk,
  -- but I haven't seen that yet.
  return $ BANKEntry (fromIntegral bankNum) (fromIntegral instCount) bs

getSAMP :: Word32 -> Get [SAMPEntry]
getSAMP n = case quotRem n 22 of
  (samps, 0) -> replicateM (fromIntegral samps) getSAMPEntry
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 22"

getINST :: Word32 -> Get [INSTEntry]
getINST n = case quotRem n 16 of
  (insts, 0) -> replicateM (fromIntegral insts) getINSTEntry
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 16"

getSDES :: Word32 -> Get [SDESEntry]
getSDES n = do
  endPosn <- (+ fromIntegral n) <$> bytesRead
  let go = bytesRead >>= \br -> case compare br endPosn of
        EQ -> return []
        LT -> do
          ent <- getSDESEntry
          (ent :) <$> go
        GT -> fail "SDES went over its chunk length"
  go

getString :: Get B.ByteString
getString = do
  len <- getWord32le
  getByteString $ fromIntegral len

getStringList :: Word32 -> Get [B.ByteString]
getStringList n = do
  endPosn <- (+ fromIntegral n) <$> bytesRead
  1 <- getWord32le -- dunno what this is
  let go = bytesRead >>= \br -> case compare br endPosn of
        EQ -> return []
        LT -> do
          str <- getString
          (str :) <$> go
        GT -> fail "SANM/SAFN went over its chunk length"
  go

riffChunks :: Get [Chunk]
riffChunks = isEmpty >>= \case
  True -> return []
  False -> do
    ctype <- getByteString 4
    size <- getWord32le
    chunk <- case ctype of
      "SAMP" -> SAMP <$> getSAMP size
      "SANM" -> SANM <$> getStringList size
      "SAFN" -> SAFN <$> getStringList size
      "BANK" -> BANK <$> getBANK size
      "BKNM" -> do
        1 <- getWord32le -- dunno
        BKNM <$> getString
      "INST" -> INST <$> getINST size
      "INNM" -> INNM <$> getStringList size
      "SDES" -> SDES <$> getSDES size
      "SDNM" -> SDNM <$> getStringList size
      _ -> fail $ "Unknown chunk type " <> show ctype
    (chunk :) <$> riffChunks

_showByteString :: B.ByteString -> String
_showByteString = intercalate " " . map showByte . B.unpack where
  showByte n = if n < 0x10 then '0' : showHex n "" else showHex n ""

vagFilter :: [(Double, Double)]
vagFilter =
  [ (0.0, 0.0)
  , (60.0 / 64.0,  0.0)
  , (115.0 / 64.0, -52.0 / 64.0)
  , (98.0 / 64.0, -55.0 / 64.0)
  , (122.0 / 64.0, -60.0 / 64.0)
  ]

decodeVAGBlock :: S.StateT (Double, Double) Get [Int16]
decodeVAGBlock = do
  bytes <- lift $ getByteString 16
  let predictor = B.index bytes 0 `shiftR` 4 -- high nibble, shouldn't be more than 4
      shift'    = B.index bytes 0 .&. 0xF    -- low  nibble
      channel   = B.index bytes 1
      samples = do
        byte <- B.unpack $ B.drop 2 bytes
        let signExtend :: Word32 -> Int32
            signExtend x = fromIntegral $ if (x .&. 0x8000) /= 0 then x .|. 0xFFFF0000 else x
            ss0 = signExtend $ (fromIntegral byte .&. 0xF ) `shiftL` 12
            ss1 = signExtend $ (fromIntegral byte .&. 0xF0) `shiftL` 8
        [   realToFrac $ ss0 `shiftR` fromIntegral shift'
          , realToFrac $ ss1 `shiftR` fromIntegral shift'
          ]
  if channel == 7
    then return []
    else forM samples $ \sample -> do
      (s0, s1) <- S.get
      let newSample = sample
            + s0 * fst (vagFilter !! fromIntegral predictor)
            + s1 * snd (vagFilter !! fromIntegral predictor)
      S.put (newSample, s0)
      -- TODO do we need to clamp this
      return $ round newSample

decodeSamples :: BL.ByteString -> [Int16]
decodeSamples bs = let
  go = decodeVAGBlock >>= \case
    []    -> return []
    block -> (block ++) <$> go
  in flip runGet bs $ do
    getByteString 16 >>= \firstRow -> if B.all (== 0) firstRow
      then return ()
      else fail "first row of VAG block not all zero"
    S.evalStateT go (0, 0)

writeWAV :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeWAV fp (A.AudioSource s r c _) = C.runConduit $ s .| C.bracketP
  (IO.openBinaryFile fp IO.WriteMode)
  IO.hClose
  (\h -> do
    let chunk ctype f = do
          let getPosn = liftIO $ IO.hGetPosn h
          liftIO $ B.hPut h ctype
          lenPosn <- getPosn
          liftIO $ B.hPut h $ B.pack [0xDE, 0xAD, 0xBE, 0xEF] -- filled in later
          HandlePosn _ start <- getPosn
          x <- f
          endPosn@(HandlePosn _ end) <- getPosn
          liftIO $ do
            IO.hSetPosn lenPosn
            writeLE h (fromIntegral $ end - start :: Word32)
            IO.hSetPosn endPosn
          return x
    chunk "RIFF" $ do
      liftIO $ B.hPut h "WAVE"
      chunk "fmt " $ liftIO $ do
        writeLE h (1                            :: Word16) -- 1 is PCM
        writeLE h (fromIntegral c               :: Word16) -- channels
        writeLE h (floor r                      :: Word32) -- sample rate
        writeLE h (floor r * fromIntegral c * 2 :: Word32) -- avg. bytes per second = rate * block align
        writeLE h (fromIntegral c * 2           :: Word16) -- block align = chans * (bps / 8)
        writeLE h (16                           :: Word16) -- bits per sample
      chunk "data" $ CL.mapM_ $ \v -> liftIO $ do
        V.forM_ v $ writeLE h
  )

class LE a where
  writeLE :: IO.Handle -> a -> IO ()

instance LE Word32 where
  writeLE h w = B.hPut h $ B.pack [a, b, c, d] where
    a = fromIntegral w
    b = fromIntegral $ w `shiftR` 8
    c = fromIntegral $ w `shiftR` 16
    d = fromIntegral $ w `shiftR` 24

instance LE Word16 where
  writeLE h w = B.hPut h $ B.pack [a, b] where
    a = fromIntegral w
    b = fromIntegral $ w `shiftR` 8

instance LE Int32 where
  writeLE h w = writeLE h (fromIntegral w :: Word32)

instance LE Int16 where
  writeLE h w = writeLE h (fromIntegral w :: Word16)

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

findSong :: FilePath -> IO (FilePath, [FilePath])
findSong dir = do
  files <- listDirectory dir
  let stripSuffix sfx xs = fmap reverse $ stripPrefix (reverse sfx) (reverse xs)
  name <- case mapMaybe (stripSuffix "_g.mid") files of
    name : _ -> return name
    []       -> error "Couldn't find <song>_g.mid in the folder"
  let mid = dir </> (name ++ "_g.mid")
      banks = map (dir </>) $ filter ((== ".bnk") . takeExtension) files
  return (mid, banks)

main :: IO ()
main = getArgs >>= \case
  ["stems", songDir] -> do
    (midPath, bnkPaths) <- findSong songDir
    Left trks <- U.decodeFile <$> Load.fromFile midPath
    sounds <- forM bnkPaths $ \bnkPath -> do
      bnk <- BL.fromStrict <$> B.readFile bnkPath
      nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
      let chunks = runGet riffChunks bnk
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
                    [ "No SDES entry found for bank"
                    , show bank
                    , "program"
                    , show prog
                    , "pitch"
                    , show pitch
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
  ["print", bnkPath] -> do
    bnk <- BL.fromStrict <$> B.readFile bnkPath
    let chunks = runGet riffChunks bnk
    putStrLn "BANK INFO"
    forM_ [x | BANK x <- chunks] print
    putStrLn ""
    putStrLn "INSTRUMENTS"
    forM_ (zip (concat [xs | INST xs <- chunks]) (concat [xs | INNM xs <- chunks])) $ \(ent, name) -> do
      print name
      print ent
      print $ _showByteString $ instBytes ent
    putStrLn ""
    putStrLn "SAMPLE DIRECTIVES"
    forM_ (zip (concat [xs | SDES xs <- chunks]) (concat [xs | SDNM xs <- chunks])) $ \(ent, name) -> do
      print name
      print ent
      print $ _showByteString $ sdesBytes ent
    putStrLn ""
    putStrLn "SAMPLES"
    forM_ (zip (concat [xs | SAMP xs <- chunks]) (concat [xs | SANM xs <- chunks])) $ \(ent, name) -> do
      print name
      print ent
  "bnk" : bnks -> forM_ bnks $ \bnkPath -> do
    bnk <- BL.fromStrict <$> B.readFile bnkPath
    nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
    let outDir = dropExtension bnkPath ++ "_samples"
    createDirectoryIfMissing False outDir
    let chunks = runGet riffChunks bnk
        samp = concat [ xs | SAMP xs <- chunks ]
        sanm = concat [ xs | SANM xs <- chunks ]
    forM_ (zip samp sanm) $ \(entry, name) -> do
      let bytes = BL.drop (fromIntegral $ sampFilePosition entry) nse
          samples = V.fromList $ decodeSamples bytes
      when (sampRate entry /= 0) $ do
        -- BL.writeFile (outDir </> B8.unpack name <.> "bin") bytes
        print (entry, name)
        runResourceT $ writeWAV (outDir </> B8.unpack name <.> "wav") $ A.AudioSource
          (C.yield samples)
          (realToFrac $ sampRate entry)
          1
          (V.length samples)
  _ -> error "incorrect usage"

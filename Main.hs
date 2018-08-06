{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad                (forM, forM_, replicateM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Control.Monad.Trans.State    as S
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 ((.|))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import qualified Data.Conduit.List            as CL
import           Data.Int
import           Data.List                    (intercalate)
import qualified Data.Vector.Storable         as V
import           Data.Word
import           GHC.IO.Handle                (HandlePosn (..))
import           Numeric
import           System.Directory             (createDirectoryIfMissing)
import           System.Environment           (getArgs)
import           System.FilePath              (dropExtension, (-<.>), (<.>),
                                               (</>))
import qualified System.IO                    as IO

data SAMPEntry = SAMPEntry
  { sampChannels     :: Int -- guessing
  , sampRate         :: Int
  , sampFilePosition :: Int
  } deriving (Eq, Show)

data Chunk
  = SAMP [SAMPEntry] -- samples
  | SANM [B.ByteString] -- sample names
  | SAFN [B.ByteString] -- sample filenames
  | BANK B.ByteString -- bank(s?)
  | BKNM B.ByteString -- bank name
  | INST [B.ByteString] -- instruments
  | INNM [B.ByteString] -- instrument names
  | SDES [B.ByteString] -- different samples? dunno
  | SDNM [B.ByteString] -- SDES names
  -- | Unknown B.ByteString BL.ByteString
  deriving (Eq, Show)

getSAMPEntry :: Get SAMPEntry
getSAMPEntry = do
  0x12 <- getWord32le -- no idea what this is
  chans <- getWord32le -- guessing this is channels, all 1 in file i'm looking at now
  rate <- getWord32le
  0 <- getWord32le -- dunno what this space is for
  0 <- getWord16le
  posn <- getWord32le
  return $ SAMPEntry (fromIntegral chans) (fromIntegral rate) (fromIntegral posn)

getSAMP :: Word32 -> Get [SAMPEntry]
getSAMP n = case quotRem n 22 of
  (samps, 0) -> replicateM (fromIntegral samps) getSAMPEntry
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 22"

getINST :: Word32 -> Get [B.ByteString]
getINST n = case quotRem n 16 of
  (insts, 0) -> replicateM (fromIntegral insts) $ getByteString 16
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 16"

getSDES :: Word32 -> Get [B.ByteString]
getSDES n = case quotRem n 33 of
  (insts, 0) -> replicateM (fromIntegral insts) $ getByteString 33
  _          -> fail $ "SDES length of " <> show n <> " not divisible by 33"

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
      "BANK" -> BANK <$> getByteString (fromIntegral size)
      "BKNM" -> do
        1 <- getWord32le -- dunno
        BKNM <$> getString
      "INST" -> INST <$> getINST size
      "INNM" -> INNM <$> getStringList size
      "SDES" -> SDES <$> getSDES size
      "SDNM" -> SDNM <$> getStringList size
      _ -> fail $ "Unknown chunk type " <> show ctype
    (chunk :) <$> riffChunks

showByteString :: B.ByteString -> String
showByteString = intercalate " " . map showByte . B.unpack where
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
  let predictor = 0 -- B.index bytes 0 `shiftR` 4 -- high nibble, shouldn't be more than 4
      shift'    = B.index bytes 0 .&. 0xF    -- low  nibble
      channel   = B.index bytes 1
      samples = do
        i <- [0, 2 .. 26]
        let numb = quot i 2 + 2
            signExtend :: Word32 -> Word32
            signExtend x = if (x .&. 0x8000) /= 0 then x .|. 0xFFFF0000 else x
            ss0 = signExtend $ (fromIntegral (B.index bytes numb) .&. 0xF ) `shiftL` 12
            ss1 = signExtend $ (fromIntegral (B.index bytes numb) .&. 0xF0) `shiftL` 8
        [   realToFrac $ ss0 `shiftR` fromIntegral shift'
          , realToFrac $ ss1 `shiftR` fromIntegral shift'
          ]
  if channel == 7
    then return []
    else forM [0 .. 27] $ \i -> do
      (s0, s1) <- S.get
      let newSample = (samples !! i)
            + s0 * fst (vagFilter !! fromIntegral predictor)
            + s1 * snd (vagFilter !! fromIntegral predictor)
      S.put (newSample, s0)
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

main :: IO ()
main = getArgs >>= \bnks -> forM_ bnks $ \bnkPath -> do
  bnk <- BL.fromStrict <$> B.readFile bnkPath
  nse <- BL.fromStrict <$> B.readFile (bnkPath -<.> "nse")
  let outDir = dropExtension bnkPath ++ "_samples"
  createDirectoryIfMissing False outDir
  let chunks = runGet riffChunks bnk
      samp = concat [ xs | SAMP xs <- chunks ]
      sanm = concat [ xs | SANM xs <- chunks ]
      sampleBytes = map (\entry -> BL.drop (fromIntegral $ sampFilePosition entry) nse) samp
  forM_ (zip3 samp sanm sampleBytes) $ \(entry, name, bytes) -> do
    let samples = V.fromList $ decodeSamples bytes
    -- BL.writeFile (outDir </> B8.unpack name <.> "bin") bytes
    runResourceT $ writeWAV (outDir </> B8.unpack name <.> "wav") $ A.AudioSource
      (C.yield samples)
      (realToFrac $ sampRate entry)
      1
      (V.length samples)

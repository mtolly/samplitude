{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Audio where

import           Control.Monad                (forM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Control.Monad.Trans.State    as S
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 ((.|))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import qualified Data.Conduit.List            as CL
import           Data.Int
import qualified Data.Vector.Storable         as V
import           Data.Word
import           GHC.IO.Handle                (HandlePosn (..))
import qualified System.IO                    as IO

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

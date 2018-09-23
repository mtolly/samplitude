{-# LANGUAGE LambdaCase #-}
module Reaper (rpp, block, line, writeRPP, processTempoTrack, tempoTrack, insertAudio) where

import           Control.Monad                    (forM_)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Writer       (WriterT, execWriterT, tell)
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (foldl')
import           Data.Maybe                       (listToMaybe)
import           Numeric                          (showFFloat)
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension, takeFileName)
import           Text.PrettyPrint                 ((<+>))
import qualified Text.PrettyPrint                 as PP

data Element = Element String [String] (Maybe [Element])
  deriving (Eq, Ord, Show, Read)

writeRPP :: (MonadIO m) => FilePath -> Element -> m ()
writeRPP path = liftIO . writeFile path . PP.render . showElement

showElement :: Element -> PP.Doc
showElement (Element k ks Nothing) = foldl' (<+>) (showAtom k) (map showAtom ks)
showElement (Element k ks (Just sub)) = let
  start = PP.char '<' <> foldl' (<+>) (showAtom k) (map showAtom ks)
  sublines = PP.nest 2 $ PP.vcat $ map showElement sub
  end = PP.char '>'
  in PP.vcat [start, sublines, end]

showAtom :: String -> PP.Doc
showAtom s = PP.text $ case (elem '"' s, elem '\'' s, elem ' ' s) of
  (True, True, _)       -> "`" ++ map removeTick s ++ "`"
  (False, False, False) -> s
  (False, _, True)      -> wrap '"'
  (False, True, False)  -> wrap '"'
  (True, False, _)      -> wrap '\''
  where wrap c = [c] ++ s ++ [c]
        removeTick = \case '`' -> '\''; c -> c

----------------------

line :: (Monad m) => String -> [String] -> WriterT [Element] m ()
line k atoms = tell [Element k atoms Nothing]

block :: (Monad m) => String -> [String] -> WriterT [Element] m () -> WriterT [Element] m ()
block k atoms sub = do
  sublines <- lift $ execWriterT sub
  tell [Element k atoms $ Just sublines]

rpp :: (Monad m) => String -> [String] -> WriterT [Element] m () -> m Element
rpp k atoms sub = do
  sublines <- execWriterT sub
  return $ Element k atoms $ Just sublines

processTempoTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t (Meta.Tempo, Maybe (Int, Int))
processTempoTrack = go 500000 . RTB.collectCoincident where
  go tempo rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> let
      newTempo = listToMaybe [ t          | E.MetaEvent (Meta.SetTempo t     ) <- evts ]
      newSig   = listToMaybe [ (n, 2 ^ d) | E.MetaEvent (Meta.TimeSig n d _ _) <- evts ]
      in case (newTempo, newSig) of
        (Nothing, Nothing)  -> RTB.delay dt $ go tempo rtb'
        (Just tempo', _)    -> RTB.cons dt (tempo', newSig) $ go tempo' rtb'
        (Nothing, Just sig) -> RTB.cons dt (tempo, Just sig) $ go tempo rtb'

tempoTrack :: (Monad m) =>
  ATB.T U.Seconds (Meta.Tempo, Maybe (Int, Int)) -> WriterT [Element] m ()
tempoTrack trk = block "TEMPOENVEX" [] $ do
  forM_ (ATB.toPairList trk) $ \(posn, (uspqn, tsig)) -> do
    let secs, bpm :: Double
        secs = realToFrac posn
        bpm = 60000000 / fromIntegral uspqn
    line "PT" $ [showDouble secs, showDouble bpm, "1"] ++ case tsig of
      Nothing           -> []
      Just (num, denom) -> [show $ num + denom * 0x10000, "0", "1"]

showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

insertAudio :: (Monad m) => U.Seconds -> U.Seconds -> Int -> Double -> Double -> FilePath -> WriterT [Element] m ()
insertAudio posn len semis pan vol path = let
  name = takeFileName path
  in block "ITEM" [] $ do
    line "POSITION" [showDouble (realToFrac posn :: Double)]
    line "LOOP" ["0"]
    line "LENGTH" [showDouble (realToFrac len :: Double)]
    line "NAME" [name]
    line "VOLPAN"
      [ "1" -- dunno
      , showDouble pan -- -1 (L) to 1 (R)
      , showDouble vol -- gain ratio, 1 is untouched, 0 is -inf
      , "-1" -- dunno
      ]
    line "PLAYRATE"
      [ "1" -- speed (new bpm / original bpm)
      , "1" -- bool: preserve pitch when changing rate
      , show semis -- semitones, can be fractional
      , "-1" -- library to use for time/pitch stretching, -1 is project default
      , "0" -- 2 = optimize for tonal content, 0 = no
      , "0.0025" -- stretch marker fade size in seconds
      ]
    let fmt = case map toLower $ takeExtension path of
          ".wav" -> "WAVE"
          ".mp3" -> "MP3"
          ".ogg" -> "VORBIS"
          ".flac" -> "FLAC"
          _ -> error $ "While generating a Reaper project: I don't know the audio format of this file: " ++ show path
    block "SOURCE" [fmt] $ do
      line "FILE" [path]

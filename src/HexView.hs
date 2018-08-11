module HexView where

import qualified Data.ByteString as B
import           Data.List       (intercalate)
import           Numeric

newtype HexView = HexView B.ByteString
  deriving (Eq, Ord)

instance Show HexView where
  show (HexView bs) = "{\n" ++ showByteString bs ++ "\n}"

showByteString :: B.ByteString -> String
showByteString = intercalate " " . map showByte . B.unpack where
  showByte n = if n < 0x10 then '0' : showHex n "" else showHex n ""

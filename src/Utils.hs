module Utils
    ( toHex
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Numeric
import           Data.List.Split
import           Data.Word (Word8)

toHex :: ByteString -> String
toHex = concat . map showHex' . B.unpack
  where
    showHex' x
      | x < 16 = "0" ++ (flip showHex "" $ x)
      | otherwise = (flip showHex "" $ x)

module Utils
    ( toHex
    , quickMeta
    , fromMeta
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Numeric
import           Data.List.Split
import           Data.Word (Word8)
import qualified Data.Aeson.Types as V
import qualified Data.Text as T

toHex :: ByteString -> String
toHex = concat . map showHex' . B.unpack
  where
    showHex' x
      | x < 16 = "0" ++ (flip showHex "" $ x)
      | otherwise = (flip showHex "" $ x)

quickMeta :: String -> ByteString -> (T.Text, V.Value)
quickMeta name bs = (T.pack name, V.String . T.pack . show $ bs)

fromMeta :: V.Value -> ByteString
fromMeta (V.String str) = read . T.unpack $ str
fromMeta _ = error "Invalid Data.Aeson.Types.Value given to fromMeta!"

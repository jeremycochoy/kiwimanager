{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( toHex
    , quickMeta
    , fromMeta
    , empty2Nothing
    ) where

import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import           Numeric
import           Data.List.Split
import           Data.Word (Word8)
import qualified Data.Aeson.Types as V
import qualified Data.Text as T
import qualified Snap.Snaplet.Auth as A
import qualified Heist.Interpreted as I
import           Heist.Splices

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

empty2Nothing :: ByteString -> Maybe ByteString
empty2Nothing bs = if (bs == B.empty) then Nothing else Just bs

{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Configuration,
    socketHost,
    socketPort,
    socketTimeout,
    socketDelay,
    gameName,
    sessionTimeout,
    sessionCookieName,
    minUserLen,
    maxUserLen,
    minPasswordLen,
    defaultConfiguration,
  ) where

import           Data.ByteString (ByteString)

data Configuration = Configuration
    { socketHost        :: String
      -- ^ The host name of the server that will be checked
    , socketPort        :: Int
      -- ^ At wich port the server should be asked
    , socketTimeout     :: Int
      -- ^ Socket timeout (in ms)
    , socketDelay       :: Int
      -- ^ Delay befor checking again the server's status (in seconds)

    , gameName          :: String
      -- ^ Name of the game/server

    , sessionTimeout    :: Int
      -- ^ Time in seconds before the session was closed
    , sessionCookieName :: ByteString
      -- ^ Name of the (crypted) cookie used to store the session

    , minUserLen        :: Int
    , maxUserLen        :: Int
    , minPasswordLen    :: Int
    }

defaultConfiguration = Configuration
    { socketHost    = "localhost"
    , socketPort    = 4542
    , socketTimeout = 100
    , socketDelay   = 60

    , gameName    = "Kiwi Beta"

    , sessionTimeout    = 3600
    , sessionCookieName = "kiwi_cookie"

    , minUserLen     = 4
    , maxUserLen     = 20
    , minPasswordLen = 5
    }

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
    defaultConfiguration,
  ) where

import           Data.ByteString (ByteString)

data Configuration = Configuration
    -- | The host name of the server that will be checked
    { socketHost        :: String
    -- | At wich port the server should be asked
    , socketPort        :: Int
    -- | Socket timeout (in ms)
    , socketTimeout     :: Int
    -- | Delay befor checking again the server's status (in seconds)
    , socketDelay       :: Int

    , gameName          :: String

    , sessionTimeout    :: Int
    , sessionCookieName :: ByteString
    }

defaultConfiguration = Configuration
    { socketHost    = "localhost"
    , socketPort    = 4542
    , socketTimeout = 100
    , socketDelay   = 60

    , gameName    = "Kiwi Beta"

    , sessionTimeout    = 3600
    , sessionCookieName = "kiwi_cookie"
    }

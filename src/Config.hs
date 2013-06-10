{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Configuration (Configuration),
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
    usernameRegex,
    databaseUrl,
    userTable,
    usernameField,
    charactersTable
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

    , usernameRegex     :: String
      -- ^ Regexp that username should match
    , minUserLen        :: Int
      -- ^ Min username length (included)
    , maxUserLen        :: Int
      -- ^ Max username length (included)
    , minPasswordLen    :: Int
      -- ^ Min password length

    , databaseUrl       :: String
      -- ^ Host / file to get the database
    , userTable         :: String
      -- ^ Table were user are stored
    , usernameField     :: String
      -- ^ name of the username field
    , charactersTable   :: String
      -- ^ name of the characters table
    , characterTable    :: String
      -- ^ Table were user are stored
    }

defaultConfiguration = Configuration
    { socketHost    = "localhost"
    , socketPort    = 4542
    , socketTimeout = 100
    , socketDelay   = 60

    , gameName    = "Kiwi Beta"

    , sessionTimeout    = 3600
    , sessionCookieName = "kiwi_cookie"

    , usernameRegex  = "^([a-zA-Z1-9_-]+)$"
    , minUserLen     = 4
    , maxUserLen     = 20
    , minPasswordLen = 5

    , databaseUrl     = "../server/db/users.s3db"
    , userTable       = "users"
    , usernameField   = "name"
    , charactersTable = "characters"
    }

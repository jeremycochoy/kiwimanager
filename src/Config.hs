{-# LANGUAGE OverloadedStrings #-}

module Config
  ( config,
  ) where

import KiwiConfiguration

config = defaultConfiguration
    { socketHost    = "localhost"
    , socketPort    = 4542

    , gameName    = "Kiwi Manager"

    , sessionTimeout    = 3600
    , sessionCookieName = "kiwi_cookie"

    , usernameRegex  = "^([a-zA-Z1-9_-]+)$"
    , minUserLen     = 4
    , maxUserLen     = 20
    , minPasswordLen = 5

    , databaseUrl     = "../server/db/users.s3db"
    , userTable       = "users"
    , usernameField   = "name"
    , characterTable = "characters"
    }

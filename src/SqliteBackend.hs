module SqliteBackend
    ( SqliteKiwiBackend (SqliteKiwiBackend)
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3

import KiwiAuthManager


data SqliteKiwiBackend = SqliteKiwiBackend
                             { sqliteFile     :: String
                             , userTable      :: String
                             , characterTable :: String
                             }

instance KiwiAuthBackend SqliteKiwiBackend where
  register = error "register not yet implemented"
  lookupByName = error "lookupByName not yet implemented"
  lookupById = error "lookupById not yet implemented"
  delete = error "delete not yet implemented"

module SqliteBackend
    ( SqliteKiwiAuthBackend (SqliteKiwiAuthBackend)
    ) where

import KiwiAuthManager

data SqliteKiwiAuthBackend = SqliteKiwiAuthBackend

instance KiwiAuthBackend SqliteKiwiAuthBackend where


{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module SqliteBackend
    ( initSqliteKiwiBackend
    ) where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Snap.Snaplet.Auth
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as V
import qualified Data.Text.Encoding as E
import           Data.ByteString (ByteString)

import           KiwiAuthManager


data SqliteKiwiBackend = SqliteKiwiBackend
                             { connection     :: Connection
                             , userTable      :: String
                             , usernameField  :: String
                             , characterTable :: String
                             }
initSqliteKiwiBackend :: FilePath
                         -- ^ File where is the user database
                      -> String
                         -- ^ Name of the user table
                      -> String
                         -- ^ Username field
                      -> String
                         -- ^ Name of the character table
                      -> IO (SqliteKiwiBackend)

------------------------------------------------------------------------------
-- | Create a Sqlite KiwiBackend, used to read/write data about
--   characters/users in the database. If we want to change the database
--   later, we will only need to modify this file.
initSqliteKiwiBackend path uT uF cT = do
    conn <- connectSqlite3 path
    return SqliteKiwiBackend
           { connection = conn
           , userTable = uT
           , usernameField = uF
           , characterTable = cT
           }

getUserByName :: SqliteKiwiBackend
                 -- ^ Kiwi Backend
              -> T.Text
                 -- ^ User name
              -> IO (Maybe AuthUser)
getUserByName SqliteKiwiBackend{..} name = do
  rows <- quickQuery' connection query [toSql . T.unpack $ name]
  case rows of
    [] -> return Nothing
    row : _ -> let [id, password, email, salt, lli, lla, ca] = row in
      return $ Just defAuthUser
        { userId = Just UserId {unUid = T.pack . show $ (fromSql id :: Int)}
        , userLogin = name
        , userPassword = Just . ClearText . fromSql $ password
        , userEmail = fromSql email
        , userMeta = HM.fromList [(T.pack "salt", V.String . T.pack . show $ (fromSql salt :: ByteString))]
        }
  where
    query = "SELECT `id`, `password`, `email`, `salt`, `lastLoginIp`, `lastLoginAt`, `createdAt` FROM `" ++ userTable ++ "` WHERE `"++ usernameField ++"`=?"

instance KiwiAuthBackend SqliteKiwiBackend where
  --TODO : do not allow any characters for field username
  register = error "register not yet implemented"
  lookupByName = getUserByName
  lookupById = error "lookupById not yet implemented"
  delete = error "delete not yet implemented"


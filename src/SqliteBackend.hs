{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module SqliteBackend
    ( initSqliteKiwiBackend
    ) where

import           Control.Monad
import           Control.Monad.State
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Snap.Snaplet.Auth
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as V
import qualified Data.Text.Encoding as E
import           Data.ByteString (ByteString)

import           KiwiAuthManager
import           Utils


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
  _ <- liftIO $ print "getUserLogin"
  rows <- quickQuery' connection query [toSql . T.unpack $ name]
  computeRows rows
  where
    query = "SELECT `id`, `name`, `password`, `email`, `salt`, `lastLoginIp`, `lastLoginAt`, `createdAt` FROM `" ++ userTable ++ "` WHERE `"++ usernameField ++"`=?"

getUserById :: SqliteKiwiBackend
                 -- ^ Kiwi Backend
              -> UserId
                 -- ^ User name
              -> IO (Maybe AuthUser)
getUserById SqliteKiwiBackend{..} id = do
  _ <- liftIO $ print "getUserID"
  rows <- quickQuery' connection query [toSql (read . T.unpack . unUid $ id :: Int)]
  computeRows rows
  where
    query = "SELECT `id`, `name`, `password`, `email`, `salt`, `lastLoginIp`, `lastLoginAt`, `createdAt` FROM `" ++ userTable ++ "` WHERE `id`=?"


computeRows :: [[SqlValue]] -> IO (Maybe AuthUser)
computeRows rows = case rows of
  [] -> return Nothing
  row : _ -> let [id, name, password, email, salt, lli, lla, ca] = row in
    return $ Just defAuthUser
        { userId = Just UserId {unUid = T.pack . show $ (fromSql id :: Int)}
        , userLogin = fromSql name
        , userPassword = Just . ClearText . fromSql $ password
        , userEmail = fromSql email
        , userMeta = HM.fromList ["salt" `quickMeta` fromSql salt]
        }


instance KiwiAuthBackend SqliteKiwiBackend where
  --TODO : do not allow any characters for field username
  register = error "register not yet implemented"
  lookupByName = getUserByName
  lookupById = getUserById
  delete = error "delete not yet implemented"


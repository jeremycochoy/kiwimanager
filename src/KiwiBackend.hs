{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module KiwiBackend
    ( KiwiBackend,
      initSqliteKiwiBackend,
      getUserInfos,
      addUser,
      getUserByName,
      getUserById,
      getCharacters,
    ) where

import           Control.Monad
import           Control.Monad.State
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Snap.Snaplet.Auth
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as V
import qualified Data.Text.Encoding as E
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Character


import           Utils
import           Config


data KiwiBackend = KiwiBackend
                             { connection     :: Connection
                             , userTable      :: String
                             , characterTable :: String
                             }
initSqliteKiwiBackend :: FilePath
                         -- ^ File where is the user database
                      -> String
                         -- ^ Name of the user table
                      -> String
                         -- ^ Name of the character table
                      -> IO (KiwiBackend)

------------------------------------------------------------------------------
-- | Create a Sqlite KiwiBackend, used to read/write data about
--   characters/users in the database. If we want to change the database
--   later, we will only need to modify this file.
initSqliteKiwiBackend url uT cT = do
    conn <- connectPostgreSQL url
    return KiwiBackend
           { connection = conn
           , userTable = uT
           , characterTable = cT
           }

getUserByName :: KiwiBackend
                 -- ^ Kiwi Backend
              -> T.Text
                 -- ^ User name
              -> IO (Maybe AuthUser)
getUserByName KiwiBackend{..} name = do
  _ <- liftIO $ print "getUserLogin"
  rows <- quickQuery' connection query [toSql . T.unpack $ name]
  computeRows rows
  where
    query = "SELECT id, name, password, email, salt, last_login_ip, last_login_at, created_at FROM " ++ userTable ++ " WHERE name=?"

getUserById :: KiwiBackend
                 -- ^ Kiwi Backend
              -> UserId
                 -- ^ User name
              -> IO (Maybe AuthUser)
getUserById KiwiBackend{..} id = do
  _ <- liftIO $ print "getUserID"
  rows <- quickQuery' connection query [toSql (read . T.unpack . unUid $ id :: Int)]
  computeRows rows
  where
    query = "SELECT id, name, password, email, salt, last_login_ip, last_login_at, created_at FROM " ++ userTable ++ " WHERE id=?"


computeRows :: [[SqlValue]] -> IO (Maybe AuthUser)
computeRows rows = case rows of
  [] -> return Nothing
  row : _ -> let [id, name, password, email, salt, lli, lla, ca] = row in do
    return $ Just defAuthUser
        { userId = Just UserId {unUid = T.pack . show $ (fromSql id :: Int)}
        , userLogin = fromSql name
        , userPassword = Just . ClearText . fromSql $ password
        , userEmail = fromSql email
        , userMeta = HM.fromList ["salt" `quickMeta` (drop 2 . fromSql $ salt)]
        }

getUserInfos :: KiwiBackend -> UserId -> IO [(String, String)]
getUserInfos KiwiBackend{..} id = do
  rows <- quickQuery' connection query [toSql (read . T.unpack . unUid $ id :: Int)]
  return $ case rows of
    row : _ -> zip fields (map getVal row)
    _       -> []
  where
    getVal = either (\_ -> "") (\s -> s) . safeFromSql
    fields = ["userName", "userEmail", "userFirstName", "userLastName", "userBirthday",
              "userLastLoginIp", "userLastLoginAt", "userCreatedAt", "userVerified", "userLogged"]
    query = "SELECT name, email, first_name, last_name, birthday, last_login_ip, last_login_at, created_at, verified, logged FROM " ++ userTable ++ " WHERE id=?"

addUser :: KiwiBackend
           -- ^ Kiwi Backend
        -> T.Text
           -- ^ Username
        -> ByteString
           -- ^ Crypted password
        -> String
           -- ^ Salt
        -> T.Text
           -- ^ Email
        -> IO (Either AuthFailure AuthUser)
addUser b@KiwiBackend{..} username password salt email = do
  _ <- liftIO $ print "getUserLogin"
  -- Save user
  run connection query [toSql username, toSql password, toSql email]
  -- Load the user and it's id from
  mbAuthUser <- getUserByName b username
  commit connection
  return . Right . fromJust $ mbAuthUser
  where
    query = "INSERT INTO " ++ userTable ++ " (name, password, salt, email) VALUES (?, ?, E'\\\\x" ++ salt ++ "', ?)"

getCharacters :: KiwiBackend -> UserId -> IO [Character]
getCharacters KiwiBackend{..} userId = do
  rows <- quickQuery' connection query [toSql (read . T.unpack . unUid $ userId :: Int)]
  return $ map computeChar rows
  where
    query = "SELECT name, type, lvl, state_pts, int, str, dex, agi, "
            ++ "vit, exp, pos_x, pos_y "
            ++ "FROM " ++ characterTable ++ " WHERE user_id=?"
    computeChar [name, ctype, lvl, statePts, int, str, dex, agi, vit, exp, posX, posY] =
      Character { cName     = fromSql name
                , cLevel    = fromSql ctype
                , cType     = fromSql lvl
                , cStatePts = fromSql statePts
                , cInt      = fromSql int
                , cStr      = fromSql str
                , cDex      = fromSql dex
                , cAgi      = fromSql agi
                , cVit      = fromSql vit
                , cExp      = fromSql exp
                , cPosX     = fromSql posX
                , cPosY     = fromSql posY
                }


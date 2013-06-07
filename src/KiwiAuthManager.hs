{-# LANGUAGE OverloadedStrings #-}

module KiwiAuthManager
  (initKiwiAuthManager
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Data.ByteString (ByteString)

------------------------------------------------------------------------------
-- | Initialize a new sqlite 'AuthManager'
initKiwiAuthManager :: (KiwiAuthBackend k) =>
                         AuthSettings
                         -- ^ Authentication settings for your app
                      -> SnapletLens b SessionManager
                         -- ^ Lens into a 'SessionManager' auth
                      -> k
                      -> SnapletInit b (AuthManager b)
initKiwiAuthManager as sl k = do
  makeSnaplet
    "KiwiAuthManager"
    "A snaplet providing user authentication"
    Nothing $ liftIO $ do
      return $! AuthManager
                       { backend               = SqliteAuthManager
                       , session               = undefined
                       , activeUser            = undefined
                       , minPasswdLen          = undefined
                       , rememberCookieName    = undefined
                       , rememberPeriod        = undefined
                       , siteKey               = undefined
                       , lockout               = undefined
                       , randomNumberGenerator = undefined
                       }

------------------------------------------------------------------------------
data SqliteAuthManager = SqliteAuthManager

instance IAuthBackend SqliteAuthManager where
  save = error "Save not yet implemented"
  destroy = error "Destroy not yet implemented"
  lookupByUserId = error "lookUpByUserID not yet implemented"
  lookupByLogin = error "lookupByLogin not yet implemented"
  lookupByRememberToken = error "lookupByRememberToken not yet implemented"

------------------------------------------------------------------------------
class KiwiAuthBackend r where
  -- | Create a user from a username, a password and an email.
  --   Should return false if it failed.
  register :: r
           -> Text
              -- ^ Username
           -> ByteString
              -- ^ Not crypted password
           -> Text
              -- ^ Email
           -> IO Bool

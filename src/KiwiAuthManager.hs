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
import           Web.ClientSession (getKey)

------------------------------------------------------------------------------
-- | Initialize a new sqlite 'AuthManager'
initKiwiAuthManager :: (KiwiAuthBackend k) =>
                         AuthSettings
                         -- ^ Authentication settings for your app
                      -> SnapletLens b SessionManager
                         -- ^ Lens into a 'SessionManager' auth
                      -> k
                      -> SnapletInit b (AuthManager b)
initKiwiAuthManager s l k = do
  makeSnaplet
    "KiwiAuthManager"
    "A snaplet providing user authentication"
    Nothing $ liftIO $ do
      key <- getKey $ asSiteKey s
      rng <- liftIO mkRNG
      return $! AuthManager
                       { backend               = KiwiAuthManager
                       , session               = l
                       , activeUser            = Nothing
                       , minPasswdLen          = asMinPasswdLen s
                       , rememberCookieName    = asRememberCookieName s
                       , rememberPeriod        = asRememberPeriod s
                       , siteKey               = key
                       , lockout               = asLockout s
                       , randomNumberGenerator = rng
                       }

------------------------------------------------------------------------------
data KiwiAuthManager = KiwiAuthManager

instance IAuthBackend KiwiAuthManager where
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

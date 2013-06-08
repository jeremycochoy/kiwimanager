{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}

module KiwiAuthManager
  ( initKiwiAuthManager
  , KiwiAuthBackend (register, lookupByName, lookupById, delete)
  , registerUser
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (registerUser)
import           Snap.Snaplet.Session
import           Data.ByteString (ByteString)
import           Web.ClientSession (getKey)

------------------------------------------------------------------------------
-- | Initialize a new sqlite 'AuthManager'
initKiwiAuthManager :: (KiwiAuthBackend k) =>
                         AuthSettings
                         -- ^ Authentication settings for your app
                      -> SnapletLens b SessionManager
                         -- ^ Lens into a 'SessionManager'
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
                       { backend               = KiwiAuthManager k
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
-- | A handler that process a registering form
registerUser :: ByteString
                -- ^ Unsername field
             -> ByteString
                -- ^ Password field
             -> ByteString
                -- ^ Password confirmation field
             -> ByteString
                -- ^ E-Mail field
             -> Handler b (AuthManager b) (Either AuthFailure AuthUser)
registerUser = error "registerUser not yet implemented"

------------------------------------------------------------------------------
data KiwiAuthManager = forall k. (KiwiAuthBackend k) => KiwiAuthManager
                       { kiwiAuthBackend ::  k
                       }

instance IAuthBackend KiwiAuthManager where
  save = error "Save not yet implemented"
  destroy = error "Destroy not yet implemented"
  lookupByUserId KiwiAuthManager{..} uid = lookupById kiwiAuthBackend uid
  lookupByLogin KiwiAuthManager{..} login = lookupByName kiwiAuthBackend login
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
          -> IO (Either AuthFailure AuthUser)
  -- | Find the user with this username
  lookupByName :: r
          -> Text
             -- ^ Username
          -> IO (Maybe AuthUser)
  -- | Find the user with this ID
  lookupById :: r
          -> UserId
             -- ^ User's ID
          -> IO (Maybe AuthUser)
  -- | Delete the user from the database
  delete :: r
         -> UserId
         -- ^ User's ID
         -> IO ()

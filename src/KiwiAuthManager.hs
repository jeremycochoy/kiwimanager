module KiwiAuthManager
  (initKiwiAuthManager
  ) where

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
initKiwiAuthManager = undefined

------------------------------------------------------------------------------
data SqliteAuthManager = SqliteAuthmanager

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

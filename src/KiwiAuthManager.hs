{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}

module KiwiAuthManager
  ( initKiwiAuthManager
  , KiwiAuthBackend (register, lookupByName, lookupById, delete)
  , registerUser
  , loginUser
  ) where

import           Control.Monad.State
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Snap
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (registerUser, loginUser)
import           Snap.Snaplet.Session
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Web.ClientSession (getKey)
import           Control.Error
import qualified Data.Aeson.Types as V
import qualified Data.HashMap.Strict as HM
import qualified Crypto.Hash.SHA256 as S256
import           Numeric
import           Utils

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
-- | Modified loginUser handler from Auth, since we want to encrypt the pasword
--   with the right algorithm. (Here, it's sha256 with a given salt
loginUser :: ByteString
               -- ^ Name of the username field
            -> ByteString
               -- ^ Name of the password field
            -> Maybe ByteString
               -- ^ Name of the "remember" field  :: ByteString
            -> (AuthFailure -> Handler b (AuthManager b) ())
               -- ^ Upon failure
            -> Handler b (AuthManager b) ()
               -- ^ Upon success
            -> Handler b (AuthManager b) ()
loginUser unf pwdf remf loginFail loginSucc =
    runEitherT (loginUser' unf pwdf remf)
    >>= either loginFail (const loginSucc)

loginUser' :: ByteString
             -> ByteString
             -> Maybe ByteString
             -> EitherT AuthFailure (Handler b (AuthManager b)) AuthUser
loginUser' unf pwdf remf = do
    mbUsername <- lift $ getParam unf
    mbPassword <- lift $ getParam pwdf
    remember <- lift $ liftM (fromMaybe False)
                    (runMaybeT $
                    do field <- MaybeT $ return remf
                       value <- MaybeT $ getParam field
                       return $ value == "1")

    password <- noteT PasswordMissing $ hoistMaybe mbPassword
    username <- noteT UsernameMissing $ hoistMaybe mbUsername
    let tUsername = E.decodeUtf8 username


    mbAuthUser <- lift $ withBackend (\b -> liftIO $ lookupByLogin b tUsername)
    let authUser = maybe defAuthUser id mbAuthUser
    let salt = case HM.lookup "salt" (userMeta authUser) of
          Just (V.String salt) -> read . T.unpack $ salt :: ByteString
          _ -> B.empty

    let cPassword = hashPassword salt password

    EitherT $ loginByUsername tUsername
                              (ClearText cPassword) remember

hashPassword :: ByteString
                -- ^ Uncrypted password
             -> ByteString
                  -- ^ Salt
             -> ByteString
hashPassword salt pwd = E.encodeUtf8 . T.pack . toHex . S256.hash $ (B.append salt pwd)

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
  save r authUser = if isJust (userId authUser) then
                          return $ Right authUser
                        else
                          error "Save not yet implemented"
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

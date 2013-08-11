{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE RecordWildCards            #-}

module KiwiAuthManager
  ( initKiwiAuthManager
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
import qualified Snap.Snaplet.Auth as A
import           Snap.Snaplet.Session
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Web.ClientSession (getKey)
import           Control.Error
import qualified Data.Aeson.Types as V
import qualified Data.HashMap.Strict as HM
import qualified Crypto.Hash.SHA256 as S256
import           Numeric
import           System.Entropy (getEntropy)
import qualified Text.Email.Validate as EMail
import           Text.Regex.Posix

import           Utils
import qualified Types as K
import           KiwiConfiguration
import           KiwiBackend

------------------------------------------------------------------------------
-- | Initialize a new 'AuthManager'.
initKiwiAuthManager :: AuthSettings
                         -- ^ Authentication settings for your app
                      -> SnapletLens b SessionManager
                         -- ^ Lens into a 'SessionManager'
                      -> KiwiBackend
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

    password <- noteT A.PasswordMissing $ hoistMaybe mbPassword
    username <- noteT A.UsernameMissing $ hoistMaybe mbUsername
    let tUsername = E.decodeUtf8 username


    mbAuthUser <- lift $ withBackend (\b -> liftIO $ lookupByLogin b tUsername)

    authUser <- noteT A.UserNotFound $ hoistMaybe mbAuthUser

    --TODO: Rewrite using our own error type!
    salt <- noteT (AuthError "salt") . hoistMaybe $
            fromMeta <$> HM.lookup "salt" (userMeta authUser)
    let cPassword = hashPassword salt password

    error. show $ (password, userPassword authUser, salt, cPassword)
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
registerUser :: Configuration
                -- ^ KiwiManager's configuration
             -> ByteString
                -- ^ Unsername field
             -> ByteString
                -- ^ Password field
             -> ByteString
                -- ^ Password confirmation field
             -> ByteString
                -- ^ E-Mail field
            -> (K.RegisterFailure -> Handler b (AuthManager b) ())
               -- ^ Upon failure
            -> (AuthUser -> Handler b (AuthManager b) ())
               -- ^ Upon success
            -> Handler b (AuthManager b) ()
registerUser cfg unf pwdf pwdcf ef regFail regSucc =
    runEitherT (registerUser' cfg unf pwdf pwdcf ef)
    >>= either regFail regSucc

registerUser' :: Configuration
                 -- ^ KiwiManager's configuration
              -> ByteString
                -- ^ Unsername field
              -> ByteString
                -- ^ Password field
              -> ByteString
                -- ^ Password confirmation field
              -> ByteString
                -- ^ E-Mail field
              -> EitherT K.RegisterFailure (Handler b (AuthManager b)) AuthUser
registerUser' Configuration{..} unf pwdf pwdcf ef = do
    mbUsername             <- lift $ (fmap E.decodeUtf8 . empty2Nothing =<<) <$> getParam unf
    mbClearPassword        <- lift $ (empty2Nothing =<<) <$> getParam pwdf
    mbClearConfirmPassword <- lift $ (empty2Nothing =<<) <$> getParam pwdcf
    mbEmail                <- lift $ (empty2Nothing =<<) <$> getParam ef

    username             <- noteT K.UsernameMissing $ hoistMaybe mbUsername
    clearPassword        <- noteT K.PasswordMissing $ hoistMaybe mbClearPassword
    clearConfirmPassword <- noteT K.PasswordMissing $ hoistMaybe mbClearConfirmPassword
    bsEmail              <- noteT K.EmailMissing    $ hoistMaybe mbEmail

    let uLength = T.length username
    _ <- noteT K.UsernameTooShort  . hoistMaybe $ mfilter (>= minUserLen) (Just uLength)
    _ <- noteT K.UsernameTooLong   . hoistMaybe $ mfilter (<= maxUserLen) (Just uLength)
    _ <- noteT K.PasswordTooShort  . hoistMaybe $ mfilter (>= minPasswordLen)
         (Just . B.length $ clearPassword)
    _ <- noteT K.UsernameIllformed . hoistMaybe $ mfilter (=~ usernameRegex)
         (Just . T.unpack $ username)

    email <- noteT K.EmailIllformed . hoistMaybe $
             E.decodeUtf8 <$> mfilter EMail.isValid (Just bsEmail)

    -- Check if no user with the same login exists
    mbUser <- lift $ withBackend (\b -> liftIO $ lookupByLogin b username)
    _ <- noteT K.UsernameUsed . hoistMaybe $ case mbUser of
        Nothing -> Just ()
        Just  _ -> Nothing

    -- Create the salt
    salt <- liftIO $ getEntropy 256

    -- Hash password with the new salt, only if password confirmed
    cryptedPassword <- noteT K.PasswordNotConfirmed . hoistMaybe $
         if (clearPassword == clearConfirmPassword)
         then Just (hashPassword salt clearPassword)
         else Nothing

    -- Create a new auth user
    let authUser = defAuthUser
               -- Nothing means "save the new user"
               { userId = Nothing
               , userEmail = Just email
               , userLogin = username
               -- We are lying, the password is actualy encrypted
               , userPassword = Just $ ClearText cryptedPassword
               , userMeta = HM.fromList ["salt" `quickMeta` salt]
               }

    eAuth <- lift $ saveUser authUser
    case eAuth of
      Left _ -> left K.UnknownRegisterError
      Right auth -> right auth

------------------------------------------------------------------------------
data KiwiAuthManager = KiwiAuthManager { kiwiAuthBackend ::  KiwiBackend }

------------------------------------------------------------------------------
-- | Create the user if the field ID is empty. Otherwise, do nothing.
--   It assume that userEmail, userPassword, and salt fields aren't empty.
saveImp :: KiwiAuthManager
        -> AuthUser
        -> IO (Either AuthFailure AuthUser)
saveImp KiwiAuthManager{..} authUser =
  if isJust (userId authUser) then
    return $ Right authUser
  else
    addUser
    kiwiAuthBackend
    login
    password
    (fromMeta metaSalt)
    email
  where
    login = userLogin authUser
    Just (ClearText password) = userPassword authUser
    Just email = userEmail authUser
    Just metaSalt = HM.lookup "salt" (userMeta authUser)

instance IAuthBackend KiwiAuthManager where
  save = saveImp
  destroy = error "Destroy not yet implemented"
  lookupByUserId KiwiAuthManager{..} uid = getUserById kiwiAuthBackend uid
  lookupByLogin KiwiAuthManager{..} login = getUserByName kiwiAuthBackend login
  lookupByRememberToken = error "lookupByRememberToken not yet implemented"

{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Snap
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Auth
import qualified Snap.Snaplet.Auth as A
import           Snap.Util.FileServe
import           Heist
import           Heist.Interpreted
import           Snap.Snaplet.Session.Backends.CookieSession

import qualified Data.Text as T
import           Data.IORef
------------------------------------------------------------------------------
import           Application
import           Config
import           Status
import qualified KiwiAuthManager as KAM
import           SqliteBackend
import qualified Types as K

------------------------------------------------------------------------------
-- | Display a login form with error messages
handleLoginError :: (HasHeist b) => AuthFailure -> Handler b (AuthManager b) ()
handleLoginError authFailure = do
    heistLocal (bindSplices splices) $ render "login"
  where
    splices = [("error_message", textSplice err)]
    err = case authFailure of
      A.PasswordMissing   -> "Your password is missing."
      A.AuthError s       -> T.pack s
      A.BackendError      -> "Internal backend error."
      A.UserNotFound      -> "Invalid username."
      A.UsernameMissing   -> "Username missing."
      A.IncorrectPassword -> "Password incorect."
      A.UserNotFound      -> "User not found."
      A.LockedOut _       -> "You are locked out for a short time."
      _                 -> "Unknown error."


------------------------------------------------------------------------------
-- | Display a login form and/or login the user
handleLogin :: Handler App (AuthManager App) ()
handleLogin = method GET handleForm <|> handleFormSubmit
  where
    handleForm = render "login"
    handleFormSubmit = do
      KAM.loginUser "login" "password" Nothing
        handleLoginError
        (redirect "/")


------------------------------------------------------------------------------
-- | Display a login form with error messages
handleRegisterError :: (HasHeist b) => K.RegisterFailure -> Handler b (AuthManager b) ()
handleRegisterError authFailure = do
    heistLocal (bindSplices splices) $ render "register"
  where
    splices = [("error_message", textSplice err)]
    err = case authFailure of
      K.PasswordMissing      -> "Your password is missing."
      K.UsernameMissing      -> "Your username is missing."
      K.UsernameUsed         -> "Username already used."
      K.EmailMissing         -> "Your email is missing."
      K.UsernameIllformed    -> "You username is invalid."
      K.EmailIllformed       -> "Your e-mail is invalid."
      K.UsernameTooShort     -> "Your username is too short."
      K.UsernameTooLong      -> "Your username is too long."
      K.PasswordTooShort     -> "Your password is too short."
      K.PasswordNotConfirmed -> "Your two password doesn't match."
      K.UnknownRegisterError -> "Unknown error."

------------------------------------------------------------------------------
-- | Display a register form and/or register the user
handleRegister :: Configuration -> Handler App (AuthManager App) ()
handleRegister conf = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "register"
    handleFormSubmit = do
      KAM.registerUser conf "login" "password" "confirm_password" "email"
        handleRegisterError
        (\user -> forceLogin user >> redirect "/")

------------------------------------------------------------------------------
-- | Logs out and redirects the user to "/".
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: Configuration -> [(ByteString, Handler App App ())]
routes cfg = [ ("",          serveDirectory "static")
             , ("login",     with auth handleLogin)
             , ("register",  with auth (handleRegister cfg))
             , ("logout",    with auth handleLogout)
             ]

------------------------------------------------------------------------------
-- | The application's splices.
splices :: Configuration -> [(T.Text, SnapletISplice App)]
splices conf = [ ("server_status", statusSplice)
               , ("minUserLen", intSplice $ minUserLen conf)
               , ("maxUserLen", intSplice $ maxUserLen conf)
               , ("minPasswordLen", intSplice $ minPasswordLen conf)
               ]

------------------------------------------------------------------------------
-- | A simple splice that
intSplice :: Int -> SnapletISplice App
intSplice = textSplice . T.pack . show

------------------------------------------------------------------------------
-- | The status splice
statusSplice :: SnapletISplice App
statusSplice = do
  -- TODO : Cache status
  v <- liftIO $ checkStatus defaultConfiguration
  if v then textSplice "on" else textSplice "off"

------------------------------------------------------------------------------
-- | Heist configuration (used to add splices)
kiwiHeistConfig :: Configuration -> HeistConfig (Handler App App)
kiwiHeistConfig conf = HeistConfig
                  { hcInterpretedSplices = splices conf
                  , hcLoadTimeSplices = []
                  , hcCompiledSplices = []
                  , hcAttributeSplices = []
                  , hcTemplateLocations = []
                  }

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "KiwiMonitor application." Nothing $ do
    let conf = defaultConfiguration
    -- KiwiBackend
    kiwiDB <- liftIO $ initSqliteKiwiBackend
              -- TODO : load from config
              "../server/db/users.s3db"
              "users"
              "name"
              "characters"
    -- Create snaplets
    h <- nestSnaplet "" heist $
         heistInit "templates"
    s <- nestSnaplet "sess" sess $
         initCookieSessionManager
           "site_key.txt"
           (sessionCookieName conf)
           (Just $ sessionTimeout conf)
    a <- nestSnaplet "auth" auth $
         KAM.initKiwiAuthManager
           defAuthSettings
           sess
           kiwiDB
    -- Add routes and splices
    addRoutes (routes conf)
    addConfig h (kiwiHeistConfig conf)
    addAuthSplices h auth
    -- Server status ; it's default value is False.
    st <- liftIO $ newIORef False
    -- Return a new application
    return $ App h s a st conf


{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
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
import           KiwiConfiguration as C
import           Status
import qualified KiwiAuthManager as KAM
import           KiwiBackend
import qualified Types as K
import           Utils

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
      _                   -> "Unknown error."


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
handleRegister :: Handler App (AuthManager App) ()
handleRegister = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "register"
    handleFormSubmit = do
      KAM.registerUser config "login" "password" "confirm_password" "email"
        handleRegisterError
        (\user -> forceLogin user >> redirect "/")

------------------------------------------------------------------------------
-- | Logs out and redirects the user to "/".
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

------------------------------------------------------------------------------
-- | Show account informations
handleAccount :: Handler App App ()
handleAccount = do
  mbAuthUser <- with auth $ currentUser
  case mbAuthUser of
    Nothing -> redirect "/"
    Just authUser -> case userId authUser of
      Nothing -> redirect "/"
      Just uid -> do
        let authSplices = userISplices authUser
        kiwiDB <- gets _kiwiDB
        infos  <-liftIO $ getUserInfos kiwiDB uid
        let userInfosSplices = map (\(a, b) -> (T.pack a, textSplice . T.pack $ b)) infos
        let splices = userInfosSplices ++ authSplices
        renderAccount splices
  where
    renderAccount splices = heistLocal (bindSplices splices) $ render "account"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         , ("login",     with auth handleLogin)
         , ("register",  with auth handleRegister)
         , ("logout",    with auth handleLogout)
         , ("account",   handleAccount)
         ]

------------------------------------------------------------------------------
-- | The application's splices.
splices :: [(T.Text, SnapletISplice App)]
splices = [ ("server_status", statusSplice)
          , ("minUserLen", intSplice $ minUserLen config)
          , ("maxUserLen", intSplice $ maxUserLen config)
          , ("minPasswordLen", intSplice $ minPasswordLen config)
          , ("error_message", textSplice "")
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
  st <- liftIO $ checkStatus

  -- Write/Read the serverStatus
  ioRef <- gets _serverStatus
  liftIO $ writeIORef ioRef st
  v <- liftIO$ readIORef ioRef

  -- Display image on/off
  if v then textSplice "on" else textSplice "off"

------------------------------------------------------------------------------
-- | Heist configuration (used to add splices)
kiwiHeistConfig :: HeistConfig (Handler App App)
kiwiHeistConfig = HeistConfig
                  { hcInterpretedSplices = splices
                  , hcLoadTimeSplices = []
                  , hcCompiledSplices = []
                  , hcAttributeSplices = []
                  , hcTemplateLocations = []
                  }

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "KiwiMonitor application." Nothing $ do
    -- KiwiBackend
    kiwiDB <- liftIO $ initSqliteKiwiBackend
              (databaseUrl config)
              (userTable config)
              (usernameField config)
              (characterTable config)
    -- Create snaplets
    h <- nestSnaplet "" heist $
         heistInit "templates"
    s <- nestSnaplet "sess" sess $
         initCookieSessionManager
           "site_key.txt"
           (sessionCookieName config)
           (Just $ sessionTimeout config)
    a <- nestSnaplet "auth" auth $
         KAM.initKiwiAuthManager
           defAuthSettings
           sess
           kiwiDB
    -- Add routes and splices
    addRoutes routes
    addConfig h kiwiHeistConfig
    addAuthSplices h auth
    -- Server status ; it's default value is False.
    st <- liftIO $ newIORef False
    -- Return a new application
    return $ App h s a st kiwiDB


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
import           KiwiAuthManager
import           SqliteBackend

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application's splices.
splices :: [(T.Text, SnapletISplice App)]
splices = [ ("server_status", statusSplice)
          , ("navbar",        callTemplate "_nav_non-auth" [])
          ]

------------------------------------------------------------------------------
-- | The status splice
statusSplice :: SnapletISplice App
statusSplice = do
  v <- liftIO $ checkStatus defaultConfiguration
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
    let conf = defaultConfiguration
    -- Create snaplets
    h <- nestSnaplet "" heist $
         heistInit "templates"
    s <- nestSnaplet "sess" sess $
         initCookieSessionManager
           "site_key.txt"
           (sessionCookieName conf)
           (Just $ sessionTimeout conf)
    a <- nestSnaplet "auth" auth $
         initKiwiAuthManager
           defAuthSettings
           sess
           SqliteKiwiAuthBackend
    -- Add routes and splices
    addRoutes routes
    addConfig h kiwiHeistConfig
    -- Server status ; it's default value is False.
    st <- liftIO $ newIORef False
    -- Return a new application
    return $ App h s undefined st conf


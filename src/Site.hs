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
    addConfig h (kiwiHeistConfig conf)
    addAuthSplices h auth
    -- Server status ; it's default value is False.
    st <- liftIO $ newIORef False
    -- Return a new application
    return $ App h s a st conf


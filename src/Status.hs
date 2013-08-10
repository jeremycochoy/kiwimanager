module Status
    ( checkStatus
    ) where

import           Control.Exception
import           Network.Socket

import qualified KiwiConfiguration as C
import qualified Config as C

checkStatus :: IO Bool
checkStatus = do
  handle ((\_ -> return False) :: SomeException -> IO Bool) checkStatusAux

checkStatusAux = do
  -- Get address
  addrInfo <- getAddrInfo (Just socketConfig ) (Just $ C.socketHost C.config) Nothing
  -- Set port
  let SockAddrInet _ h = (addrAddress $ head addrInfo)
  let mySockAddr = SockAddrInet (fromIntegral $ C.socketPort C.config) h

  -- Create socket and connect
  socket   <- socket AF_INET Stream defaultProtocol
  connect socket mySockAddr

  -- If no exception was throwed, then just close it
  sClose socket

  return True
  where
    socketConfig = defaultHints
      { addrFamily = AF_INET
      , addrSocketType = Stream
      }


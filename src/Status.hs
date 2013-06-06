module Status
    ( checkStatus
    ) where

import           Control.Exception

import qualified Config as C
import           Network.Socket

checkStatus :: C.Configuration -> IO Bool
checkStatus conf = do
  handle ((\_ -> return False) :: SomeException -> IO Bool) (checkStatusAux conf)

checkStatusAux conf = do
  -- Get address
  addrInfo <- getAddrInfo (Just socketConfig ) (Just $ C.socketHost conf) Nothing
  -- Set port
  let SockAddrInet _ h = (addrAddress $ head addrInfo)
  let mySockAddr = SockAddrInet (fromIntegral $ C.socketPort conf) h

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


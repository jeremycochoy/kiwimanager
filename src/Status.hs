module Status
    ( checkStatus
    ) where

import Config

checkStatus :: Configuration -> IO Bool
checkStatus conf = do
  return False

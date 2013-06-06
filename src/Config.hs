module Config
  ( Configuration,
    socketHost,
    socketPort,
    socketTimeout,
    socketDelay,
    gameName,
    defaultConfiguration,
  ) where

data Configuration = Configuration
    -- | The host name of the server that will be checked
    { socketHost    :: String
    -- | At wich port the server should be asked
    , socketPort    :: Int
    -- | Socket timeout (in ms)
    , socketTimeout :: Int
    -- | Delay befor checking again the server's status (in seconds)
    , socketDelay   :: Int

    , gameName      :: String
    }

defaultConfiguration = Configuration
    { socketHost    = "localhost"
    , socketPort    = 4542
    , socketTimeout = 100
    , socketDelay   = 60

    , gameName    = "Kiwi Beta"
    }

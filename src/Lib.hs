module Lib
    ( Config (Config)
    , proxyFromConfig
    , Proxy
    , enableProxy
    , disableProxy
    , timeoutProxy
    ) where

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Monad (forever)
import Control.Concurrent.Async (async, withAsync, concurrently_, Async, cancel)

import Control.Exception (finally)

data Proxy = Proxy {
    config :: Config
  , state  :: ProxyState
}

data Config = Config {
    listenAddr   :: SockAddr
  , upstreamAddr :: SockAddr
}

data ProxyState =
    Disabled
  | Enabled ListenHandle
  | Timeout Socket

type ListenHandle = Async ()

proxyFromConfig :: Config -> Proxy
proxyFromConfig config = Proxy config Disabled

localhost = tupleToHostAddress (127, 0, 0, 1)

forwardStream :: Socket -> Socket -> IO ()
forwardStream from to =
  forever $
    do content <- recv from 4096
       send to content

proxyConnection :: Socket -> SockAddr -> IO ()
proxyConnection down upstreamAddr = do
  up <- socket AF_INET Stream 0
  connect up upstreamAddr
  forward up down `finally` close up >> close down
  where
    forward :: Socket -> Socket -> IO ()
    forward up down =
      concurrently_ (forwardStream down up) (forwardStream up down)

-- TODO make sure listen loop can handle multiple connections
acceptLoop :: Socket -> SockAddr -> IO ()
acceptLoop server upstreamAddr = do
  (downstream, _) <- accept server
  withAsync (proxyConnection downstream upstreamAddr) $ \_ ->
    acceptLoop server upstreamAddr

listenOn :: SockAddr -> IO Socket
listenOn listenAddr = do
  server <- socket AF_INET Stream 0
  bind server listenAddr
  listen server 1
  return server

listenAndAccept :: Config -> IO ListenHandle
listenAndAccept (Config listenAddr upstreamAddr) = do
  server <- listenOn listenAddr
  async (acceptLoop server upstreamAddr `finally` close server)

enableProxy :: Proxy -> IO Proxy
enableProxy proxy@(Proxy _ (Enabled _)) = return proxy
enableProxy proxy@(Proxy _ (Timeout _)) = disableProxy proxy >>= enableProxy
enableProxy (Proxy config Disabled) =
  Proxy config . Enabled <$> listenAndAccept config

disableProxy :: Proxy -> IO Proxy
disableProxy proxy@(Proxy _ Disabled) = return proxy
disableProxy (Proxy config (Enabled handle)) =
  cancel handle >> return (Proxy config Disabled)
disableProxy (Proxy config (Timeout server)) =
  close server >> return (Proxy config Disabled)

timeoutProxy :: Proxy -> IO Proxy
timeoutProxy proxy@(Proxy _ (Timeout _)) = return proxy
timeoutProxy proxy@(Proxy _ (Enabled _)) = disableProxy proxy >>= timeoutProxy
timeoutProxy proxy@(Proxy config@(Config listenAddr _) Disabled) =
  Proxy config . Timeout <$> listenOn listenAddr

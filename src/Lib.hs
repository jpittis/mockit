module Lib
    ( Config (Config)
    , proxyFromConfig
    , Proxy
    , proxyAddr
    , enableProxy
    , disableProxy
    , timeoutProxy
    , proxyEnabled
    , localhost
    , proxyConnection
    ) where

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Control.Concurrent.Async (async, withAsync, concurrently)

import Control.Exception (finally)

localhost = tupleToHostAddress (127, 0, 0, 1)

forwardStream :: Socket -> Socket -> IO ()
forwardStream from to =
  forever $
    do content <- recv from 4096
       send to content

-- proxyConnection takes an open connection and an upstream address. It opens a
-- second connection to the upstream address and forwards stream data between
-- the two connections.
proxyConnection :: Socket -> SockAddr -> IO ()
proxyConnection down upstreamAddr = do
  up <- socket AF_INET Stream 0
  connect up upstreamAddr
  forward up down `finally` close up >> close down
  where
    forward up down = do
      concurrently (forwardStream down up)
                   (forwardStream up down)
      return ()

listenLoop :: Socket -> SockAddr -> IO ()
listenLoop server upstreamAddr = do
  (downstream, _) <- accept server
  withAsync (proxyConnection downstream upstreamAddr) $ \_ ->
    listenLoop server upstreamAddr

data Config = Config {
    listenAddr   :: SockAddr
  , upstreamAddr :: SockAddr
}

data Proxy = Proxy {
    config :: Config
  , state  :: ProxyState
}

data ProxyState =
    Disabled
  | Enabled Socket
  | Timeout Socket

proxyFromConfig :: Config -> Proxy
proxyFromConfig config = Proxy config Disabled

-- An Enabled proxy passes stream data between the upstream and downstream.
enableProxy :: Proxy -> IO Proxy
enableProxy proxy@(Proxy _ (Enabled _)) = return proxy
enableProxy proxy@(Proxy _ (Timeout _)) = disableProxy proxy >>= enableProxy
enableProxy (Proxy config@(Config listenAddr upstreamAddr) Disabled) = do
  server <- listenOn listenAddr
  -- TODO: ignore the thread handle for now.
  _ <- async $ listenLoop server upstreamAddr
  return $ Proxy config (Enabled server)

-- A Disabled proxy is not listening on the listenPort. Attempts to connect to
-- the listenPort will be rejected.
disableProxy :: Proxy -> IO Proxy
disableProxy proxy@(Proxy _ Disabled) = return proxy
disableProxy (Proxy config (Enabled server)) = do
  close server
  return (Proxy config Disabled)
disableProxy (Proxy config (Timeout server)) = do
  close server
  return (Proxy config Disabled)

-- A Timeout proxy is listen on it's listen port but is not accepting
-- connections. Any data sent will never reach the upstream.
timeoutProxy :: Proxy -> IO Proxy
timeoutProxy proxy@(Proxy _ (Timeout _)) = return proxy
timeoutProxy proxy@(Proxy _ (Enabled _)) = disableProxy proxy >>= timeoutProxy
timeoutProxy proxy@(Proxy config@(Config listenAddr _) Disabled) = do
  server <- listenOn listenAddr
  return $ Proxy config (Enabled server)

listenOn :: SockAddr -> IO Socket
listenOn listenAddr = do
  server <- socket AF_INET Stream 0
  bind server listenAddr
  listen server 1
  return server

proxyAddr :: Proxy -> SockAddr
proxyAddr (Proxy (Config listenAddr _) _) = listenAddr

proxyEnabled :: Proxy -> Bool
proxyEnabled (Proxy _ (Enabled _)) = True
proxyEnabled _ = False

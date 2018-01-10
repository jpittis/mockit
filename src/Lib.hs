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
    ) where

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Control.Concurrent.Async (async)

localhost = tupleToHostAddress (127, 0, 0, 1)

proxySocket :: Socket -> Socket -> IO ()
proxySocket from to =
  forever $
    do content <- recv from 4096
       send to content

proxyConnection :: Socket -> SockAddr -> IO ()
proxyConnection conn toAddr = do
  upstream <- socket AF_INET Stream 0
  connect upstream toAddr
  async $ proxySocket conn upstream
  proxySocket upstream conn

listenLoop :: Socket -> SockAddr -> IO ()
listenLoop server toAddr = do
  (conn, _) <- accept server
  async $ proxyConnection conn toAddr
  listenLoop server toAddr

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

enableProxy :: Proxy -> IO Proxy
enableProxy proxy@(Proxy _ (Enabled _)) = return proxy
enableProxy proxy@(Proxy _ (Timeout _)) = disableProxy proxy >>= enableProxy
enableProxy (Proxy config@(Config listenAddr upstreamAddr) Disabled) = do
  server <- listenOn listenAddr
  async $ listenLoop server upstreamAddr
  return $ Proxy config (Enabled server)

disableProxy :: Proxy -> IO Proxy
disableProxy proxy@(Proxy _ Disabled) = return proxy
disableProxy (Proxy config (Enabled server)) = do
  close server
  return (Proxy config Disabled)
disableProxy (Proxy config (Timeout server)) = do
  close server
  return (Proxy config Disabled)

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

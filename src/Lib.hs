module Lib
    ( Config
    , proxyFromConfig
    , Proxy
    , proxyAddr
    , enableProxy
    , disableProxy
    , proxyEnabled
    , localhost
    ) where

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)
import Control.Concurrent.Async (async)

localhost = (tupleToHostAddress (127, 0, 0, 1))

proxySocket :: Socket -> Socket -> IO ()
proxySocket from to = do
  forever $ do
    content <- recv from 4096
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

type Config = (Maybe SockAddr, SockAddr) -- TODO consider not exposing this

data Proxy = Proxy (Config, ProxyState)
data ProxyState =
    Disabled
  | Enabled (SockAddr, Socket)

proxyFromConfig :: Config -> Proxy
proxyFromConfig config = Proxy (config, Disabled)

enableProxy :: Proxy -> IO Proxy
enableProxy proxy@(Proxy (_, Enabled _)) = return proxy
enableProxy (Proxy (config@(listenAddr, upstreamAddr), Disabled)) = do
  server <- socket AF_INET Stream 0
  bind server $ fromMaybe (SockAddrInet aNY_PORT localhost) listenAddr
  listen server 1
  async $ listenLoop server upstreamAddr
  addr <- getSocketName server
  return $ Proxy (config, Enabled (addr, server))

disableProxy :: Proxy -> IO Proxy
disableProxy proxy@(Proxy (_, Disabled)) = return proxy
disableProxy (Proxy (config, Enabled (_, server))) = do
  close server
  return $ Proxy (config, Disabled)

proxyAddr :: Proxy -> Maybe SockAddr
proxyAddr (Proxy (_, Enabled (addr, _))) = Just addr
proxyAddr _ = Nothing

proxyEnabled :: Proxy -> Bool
proxyEnabled (Proxy (_, Enabled _)) = True
proxyEnabled _ = False

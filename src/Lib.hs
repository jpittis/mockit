module Lib
    ( localhost
    , Config
    , startProxy
    ) where

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Maybe (fromMaybe)

localhost = (tupleToHostAddress (127, 0, 0, 1))

type Config = (Maybe SockAddr, SockAddr)

proxySocket :: Socket -> Socket -> IO ()
proxySocket from to = do
  forever $ do
    content <- recv from 4096
    send to content

proxyConnection :: Socket -> SockAddr -> IO ()
proxyConnection conn toAddr = do
  upstream <- socket AF_INET Stream 0
  connect upstream toAddr
  forkIO $ proxySocket conn upstream
  proxySocket upstream conn

listenLoop :: Socket -> SockAddr -> IO ()
listenLoop server toAddr = do
  (conn, _) <- accept server
  forkIO $ proxyConnection conn toAddr
  listenLoop server toAddr

startProxy :: Config -> IO SockAddr
startProxy (listenAddr, upstreamAddr) = do
  server <- socket AF_INET Stream 0
  bind server $ fromMaybe (SockAddrInet aNY_PORT localhost) listenAddr
  listen server 1
  forkIO $ listenLoop server upstreamAddr
  getSocketName server

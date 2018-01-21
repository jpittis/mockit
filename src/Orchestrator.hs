module Orchestrator
    ( startOrchestrator
    , OrchReader
    , runCommand
    ) where

import qualified Api

import Proxy
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map.Strict as Map

import Control.Concurrent.MVar
import Control.Concurrent.Async (async, Async)

import Data.Text (Text)

import Control.Monad.Trans.Reader (ReaderT, ask)

import Network.Socket

data Orch = Orch { orchRequests :: MVar Request, orchHandle :: Async () }

data Request = Request { reqCommand :: Api.Command, reqResponse :: MVar Api.Response }

type ProxyMap = Map.Map Text Proxy
type Proxies a = StateT ProxyMap IO a

type OrchReader = ReaderT Orch IO

startOrchestrator :: IO Orch
startOrchestrator = do
  requests <- newEmptyMVar
  handle <- async $ requestLoop requests Map.empty
  return (Orch requests handle)
  where
    requestLoop :: MVar Request -> ProxyMap -> IO ()
    requestLoop requests state = do
      (Request command resps) <- readMVar requests
      r <- evalStateT (handleCommand command) state
      putMVar resps r
      requestLoop requests state

runCommand :: Api.Command -> OrchReader Api.Response
runCommand command = do
  (Orch requests _) <- ask
  response <- liftIO newEmptyMVar
  liftIO $ putMVar requests (Request command response)
  liftIO $ readMVar response

handleCommand :: Api.Command -> Proxies Api.Response

handleCommand (Api.Create name listenHost listenPort upstreamHost upstreamPort) = do
  config <- liftIO $ createConfig listenHost listenPort upstreamHost upstreamPort
  createProxy name config
  where
    createProxy name config = do
      proxies <- get
      let proxy = proxyFromConfig config
      proxy <- liftIO $ enableProxy proxy
      put $ Map.insert name proxy proxies
      return $ Api.SuccessResp True
    createConfig listenHost listenPort upstreamHost upstreamPort = do
      listenAddrHost <- inet_addr listenHost
      upstreamAddrHost <- inet_addr upstreamHost
      let listenAddr = SockAddrInet (fromIntegral listenPort) listenAddrHost
      let upstreamAddr = SockAddrInet (fromIntegral upstreamPort) upstreamAddrHost
      return $ Config listenAddr upstreamAddr

handleCommand (Api.Delete name) = do
  proxies <- get
  put $ Map.delete name proxies
  return $ Api.SuccessResp True

handleCommand (Api.Update name state) = do
  proxy <- findProxy name
  case proxy of
    Just proxy -> updateState proxy
    Nothing -> return $ Api.SuccessResp False
  where
    updateState proxy = do
      proxy <- liftIO $ update state proxy
      proxies <- get
      put $ Map.insert name proxy proxies
      return $ Api.SuccessResp True
    update Api.Disabled = disableProxy
    update Api.Enabled  = enableProxy
    update Api.Timeout  = timeoutProxy

handleCommand (Api.Get name) = do
  proxy <- findProxy name
  case proxy of
    Just proxy -> do
      proxy <- liftIO $ proxyToApi (name, proxy)
      return $ Api.ProxyResp proxy
    Nothing -> return $ Api.SuccessResp False

handleCommand Api.List = do
  proxies <- get
  proxies <- liftIO $ mapM proxyToApi (Map.assocs proxies)
  return $ Api.ProxiesResp proxies

findProxy :: Text -> Proxies (Maybe Proxy)
findProxy name = do
  proxies <- get
  return $ Map.lookup name proxies

proxyToApi :: (Text, Proxy) -> IO Api.Proxy
proxyToApi (name, proxy) = do
  let (Config listenAddr upstreamAddr) = proxyConfig proxy
  let (SockAddrInet (PortNum listenPort) listenHostAddr) = listenAddr
  let (SockAddrInet (PortNum upstreamPort) upstreamHostAddr) = upstreamAddr
  listenHost <- inet_ntoa listenHostAddr
  upstreamHost <- inet_ntoa upstreamHostAddr
  return $ Api.Proxy name (proxyApiState proxy) listenHost listenPort upstreamHost upstreamPort

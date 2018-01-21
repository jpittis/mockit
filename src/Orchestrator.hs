-- {-# LANGUAGE OverloadedStrings #-}

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

-- TODO: Right now this always returns True. It should fail if the proxy by
-- that name already exists.
handleCommand (Api.CreateComm (Api.Create name listenHost listenPort upstreamHost upstreamPort)) = do
  listenAddrHost <- liftIO $ inet_addr listenHost
  upstreamAddrHost <- liftIO $ inet_addr upstreamHost
  let listenAddr = SockAddrInet (read listenPort :: PortNumber) listenAddrHost
  let upstreamAddr = SockAddrInet (read upstreamPort :: PortNumber) upstreamAddrHost
  createProxy name (Config listenAddr upstreamAddr)
  return $ Api.SuccessResp True

handleCommand (Api.DeleteComm (Api.Delete name)) = do
  deleteProxy name
  return $ Api.SuccessResp True

handleCommand (Api.UpdateComm (Api.Update name state)) = do
  success <- updateProxy name state
  return $ Api.SuccessResp success

handleCommand (Api.GetComm (Api.Get name)) = do
  proxy <- findProxy name
  case proxy of
    Just p -> return $ Api.ProxyResp (proxyToApiProxy (name, p))
    Nothing -> return $ Api.SuccessResp False

handleCommand Api.ListComm = do
  proxies <- get
  return $ Api.ProxiesResp (map proxyToApiProxy (Map.assocs proxies))

proxyToApiProxy :: (Text, Proxy) -> Api.Proxy
proxyToApiProxy (name, proxy) =
  let state = Api.Disabled
      listenHost = "one"
      listenPort = 33
      upstreamHost = "three"
      upstreamPort = 44 in
  Api.Proxy name state listenHost listenPort upstreamHost upstreamPort

createProxy :: Text -> Config -> Proxies ()
createProxy name config = do
  proxies <- get
  let proxy = proxyFromConfig config 
  proxy <- liftIO $ enableProxy proxy
  put $ Map.insert name proxy proxies
  return ()

deleteProxy :: Text -> Proxies ()
deleteProxy name = do
  proxies <- get
  put $ Map.delete name proxies
  return ()

updateProxy :: Text -> Api.State -> Proxies Bool
updateProxy name state = do
  proxy <- findProxy name
  let update = case state of
                 Api.Disabled -> disableProxy
                 Api.Enabled -> enableProxy
                 Api.Timeout -> timeoutProxy
  case proxy of
    Just p -> do
      p <- liftIO $ update p
      proxies <- get
      put $ Map.insert name p proxies
      return True
    Nothing ->
      return False

findProxy :: Text -> Proxies (Maybe Proxy)
findProxy name = do
  proxies <- get
  return $ Map.lookup name proxies

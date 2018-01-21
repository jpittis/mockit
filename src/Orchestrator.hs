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

localhost = tupleToHostAddress (127, 0, 0, 1)

handleCommand (Api.CreateComm (Api.Create name listenHost listenPort upstreamHost upstreamPort)) = do
  let listenAddr = SockAddrInet (PortNum listenPort) listenHost
  let upstreamAddr = SockAddrInet (PortNum upstreamPort) upstreamHost
  createProxy name (Config listenAddr upstreamAddr)
  return $ Api.SuccessResp True

handleCommand (Api.DeleteComm (Api.Delete _)) = return $ Api.SuccessResp True

handleCommand (Api.UpdateComm (Api.Update _ _)) = return $ Api.SuccessResp True

handleCommand (Api.GetComm (Api.Get _)) = return $ Api.SuccessResp True

handleCommand Api.ListComm = return $ Api.ProxiesResp []

createProxy :: Text -> Config -> Proxies ()
createProxy name config = do
  proxies <- get
  let proxy = proxyFromConfig config 
  proxy <- liftIO $ enableProxy proxy
  put $ Map.insert name proxy proxies
  return ()

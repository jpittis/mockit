-- {-# LANGUAGE OverloadedStrings #-}

module Orchestrator
    ( startOrchestrator
    , Orch
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
handleCommand command =
  case command of
    Api.CreateComm _ -> return $ Api.SuccessResp True
    Api.DeleteComm _ -> return $ Api.SuccessResp False

createProxy :: Text -> Config -> Proxies ()
createProxy name config = do
  proxies <- get
  let proxy = proxyFromConfig config 
  proxy <- liftIO $ enableProxy proxy
  put $ Map.insert name proxy proxies
  return ()

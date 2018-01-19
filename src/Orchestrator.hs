-- {-# LANGUAGE OverloadedStrings #-}

module Orchestrator
    ( startOrchestrator
    , Orch
    ) where

import qualified Api

import Proxy
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map.Strict as Map

import Control.Concurrent.MVar
import Control.Concurrent.Async (async, Async)

import Data.Text (Text)

data Orch = Orch { orchRequests :: MVar Request , orchHandle :: Async () }

data Request = Request { reqCommand :: Command, reqResponse :: MVar Response }
type Response = Bool

data Command = Create Api.Create | Delete Api.Delete

type ProxyMap = Map.Map Text Proxy
type Proxies a = StateT ProxyMap IO a

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

handleCommand :: Command -> Proxies Response
handleCommand command =
  case command of
    Create _ -> return True
    Delete _ -> return False

createProxy :: Text -> Config -> Proxies ()
createProxy name config = do
  proxies <- get
  let proxy = proxyFromConfig config 
  proxy <- liftIO $ enableProxy proxy
  put $ Map.insert name proxy proxies
  return ()

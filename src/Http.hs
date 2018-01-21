{-# LANGUAGE OverloadedStrings #-}

module Http
    ( routes
    ) where

import Api
import Orchestrator

import Web.Scotty.Trans
import Data.Text.Lazy

import Control.Monad.Trans.Class (lift)

routes :: ScottyT Text OrchReader ()
routes = do
  get    "/proxies"       listProxiesHandler
  post   "/proxies"       createProxyHandler
  get    "/proxies/:name" getProxyHandler
  delete "/proxies/:name" deleteProxyHandler
  put    "/proxies/:name" updateProxyHandler

listProxiesHandler :: ActionT Text OrchReader ()
listProxiesHandler = do
  (ProxiesResp proxies) <- lift $ runCommand ListComm
  json proxies 

createProxyHandler :: ActionT Text OrchReader ()
createProxyHandler = do
  create <- jsonData
  (SuccessResp success) <- lift $ runCommand (CreateComm create)
  json success

getProxyHandler :: ActionT Text OrchReader ()
getProxyHandler = do
  get <- jsonData
  (ProxyResp proxy) <- lift $ runCommand (GetComm get)
  json proxy

deleteProxyHandler :: ActionT Text OrchReader ()
deleteProxyHandler = do
  delete <- jsonData
  (SuccessResp success) <- lift $ runCommand (DeleteComm delete)
  json success

updateProxyHandler :: ActionT Text OrchReader ()
updateProxyHandler = do
  update <- jsonData
  (SuccessResp success) <- lift $ runCommand (UpdateComm update)
  json success

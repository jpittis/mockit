{-# LANGUAGE OverloadedStrings #-}

module Http
    ( routes
    ) where

import Api
import Orchestrator (Orch)

import Web.Scotty

routes :: Orch -> ScottyM ()
routes orch = do
  get    "/proxies"       listProxiesHandler
  post   "/proxies"       (createProxyHandler orch)
  get    "/proxies/:name" getProxyHandler
  delete "/proxies/:name" (deleteProxyHandler orch)
  put    "/proxies/:name" updateProxyHandler

listProxiesHandler :: ActionM ()  
listProxiesHandler = json listProxies

createProxyHandler :: Orch -> ActionM ()
createProxyHandler orch = do
  command <- jsonData
  json $ createProxy command

getProxyHandler :: ActionM ()
getProxyHandler = do
  command <- jsonData
  json $ getProxy command

deleteProxyHandler :: Orch -> ActionM ()
deleteProxyHandler orch = do
  command <- jsonData
  json $ deleteProxy command

updateProxyHandler :: ActionM ()
updateProxyHandler = do
  command <- jsonData
  json $ deleteProxy command

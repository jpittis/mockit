{-# LANGUAGE OverloadedStrings #-}

module Http
    ( routes
    ) where

import Api

import Web.Scotty

routes :: ScottyM ()
routes = do
  get    "/proxies"       listProxiesHandler 
  post   "/proxies"       createProxyHandler
  get    "/proxies/:name" getProxyHandler
  delete "/proxies/:name" deleteProxyHandler
  put    "/proxies/:name" updateProxyHandler

listProxiesHandler :: ActionM ()  
listProxiesHandler = json listProxies

createProxyHandler :: ActionM ()
createProxyHandler = do
  command <- jsonData
  json $ createProxy command

getProxyHandler :: ActionM ()
getProxyHandler = do
  command <- jsonData
  json $ getProxy command

deleteProxyHandler :: ActionM ()
deleteProxyHandler = do
  command <- jsonData
  json $ deleteProxy command

updateProxyHandler :: ActionM ()
updateProxyHandler = do
  command <- jsonData
  json $ deleteProxy command

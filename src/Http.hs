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
listProxiesHandler = json listProxies

createProxyHandler :: ActionT Text OrchReader ()
createProxyHandler = do
  command <- jsonData
  -- Should use pattern matching to create the
  -- command and extract
  -- the data from the response.
  resp <- lift $ runCommand command
  json resp

getProxyHandler :: ActionT Text OrchReader ()
getProxyHandler = do
  command <- jsonData
  json $ getProxy command

deleteProxyHandler :: ActionT Text OrchReader ()
deleteProxyHandler = do
  command <- jsonData
  json $ deleteProxy command

updateProxyHandler :: ActionT Text OrchReader ()
updateProxyHandler = do
  command <- jsonData
  json $ deleteProxy command

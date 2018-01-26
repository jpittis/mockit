{-# LANGUAGE OverloadedStrings #-}

module Http
    ( serve
    ) where

import Handler (HandlerReader, runCommand, Handler)

import Api (Command(Delete, List, Get))

import Web.Scotty.Trans
import Data.Text.Lazy
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)

import Data.Default.Class (def)
import Network.Wai.Handler.Warp (setPort)

serve :: Handler s -> IO ()
serve h = do
  let readerToIO ma = runReaderT ma h
  scottyOptsT opts readerToIO routes
  where
    opts = def { verbose = 0
               , settings = setPort 3000 $ settings def
               }

routes :: ScottyT Text (HandlerReader s) ()
routes = do
  post   "/proxies"         respondToJSONBody
  put    "/proxies"         respondToJSONBody
  get    "/proxies"         respondToList
  get    "/proxies/:name" $ respondToName Get
  delete "/proxies/:name" $ respondToName Delete
  where
    respondToJSONBody = do
      command <- jsonData
      response <- lift $ runCommand command
      json response
    respondToList = do
      response <- lift $ runCommand List
      json response
    respondToName command = do
      name <- param "name"
      response <- lift $ runCommand (command name)
      json response



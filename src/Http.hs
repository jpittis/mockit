{-# LANGUAGE OverloadedStrings #-}

module Http
    ( serve
    ) where

import Handler (HandlerState, runCommand, Handler)

import Web.Scotty.Trans
import Data.Text.Lazy
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Trans.Class (lift)

serve :: Handler h => h -> IO ()
serve h = do
  let readerToIO ma = evalStateT ma h
  scottyT 3000 readerToIO routes

routes :: Handler h => ScottyT Text (HandlerState h) ()
routes = do
  get    "/proxies"       respondToCommand
  post   "/proxies"       respondToCommand
  get    "/proxies/:name" respondToCommand
  delete "/proxies/:name" respondToCommand
  put    "/proxies/:name" respondToCommand
  where
    respondToCommand = do
      command <- jsonData
      response <- lift $ runCommand command
      json response


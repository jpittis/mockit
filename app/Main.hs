{-# LANGUAGE OverloadedStrings #-}

module Main where

import Orchestrator (startOrchestrator, OrchReader, runCommand)

import Web.Scotty.Trans
import Data.Text.Lazy
import Control.Monad.Trans.Reader (runReaderT)

import Control.Monad.Trans.Class (lift)

routes :: ScottyT Text OrchReader ()
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

main :: IO ()
main = do
  orch <- startOrchestrator
  let readerToIO ma = runReaderT ma orch
  scottyT 3000 readerToIO routes

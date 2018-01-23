{-# LANGUAGE OverloadedStrings #-}

module Main where

import Handler (startHandler, HandlerReader, runCommand)

import Web.Scotty.Trans
import Data.Text.Lazy
import Control.Monad.Trans.Reader (runReaderT)

import Control.Monad.Trans.Class (lift)

routes :: ScottyT Text HandlerReader ()
routes = do
  get    "/proxies" respondToCommand
  post   "/proxies" respondToCommand
  delete "/proxies" respondToCommand
  where
    respondToCommand = do
      command <- jsonData
      response <- lift $ runCommand command
      json response

main :: IO ()
main = do
  h <- startHandler
  let readerToIO ma = runReaderT ma h
  scottyT 3000 readerToIO routes

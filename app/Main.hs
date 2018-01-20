module Main where

import Orchestrator (startOrchestrator)
import Http

import Web.Scotty.Trans (scottyT)
import Control.Monad.Trans.Reader (runReaderT)

main :: IO ()
main = do
  orch <- startOrchestrator
  let readerToIO ma = runReaderT ma orch
  scottyT 3000 readerToIO routes

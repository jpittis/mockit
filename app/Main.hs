module Main where

import Orchestrator (startOrchestrator)

import Http
import Web.Scotty (scotty)

main :: IO ()
main = do
  orch <- startOrchestrator
  scotty 3000 (routes orch)

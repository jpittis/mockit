module Main where

import Http
import Handler (startHandler, handleCommand)

import qualified Data.Map.Strict as Map

main :: IO ()
main = startHandler Map.empty handleCommand >>= serve

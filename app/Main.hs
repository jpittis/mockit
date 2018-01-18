module Main where

import Http
import Web.Scotty (scotty)

main :: IO ()
main = scotty 3000 routes

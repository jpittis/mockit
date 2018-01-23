module Main where

import Http
import Handler (startHandler, ProxyHandler)


main :: IO ()
main = (startHandler :: IO ProxyHandler) >>= serve

module Main where

import Lib

import Network.Socket (SockAddr(SockAddrInet))

import System.Environment (getArgs)
import System.Exit (die)
import Text.Read(readMaybe)

import Control.Monad (when)

-- TODO
-- ---
-- - Accept buffer and 3 way handshake timeouts.
-- - Latency.
-- - Blackhole.
-- - Provide an listen port if not privded.

-- Suggestions from Zen
-- ---
-- Consider replacing Proxy and ProxyState with records to provide better
-- documentation.
-- Run everything through hlint.
-- Read about error handing here:
-- https://hackage.haskell.org/package/managed-1.0.5/docs/Control-Monad-Managed.html
-- This is the standard logger:
-- https://hackage.haskell.org/package/fast-logger
-- Look into deriving (ToJson)
-- Check out the withX pattern. A function to run with the proxy enabled.
-- Also, learn about Functor, Applicative, Monad and MTL.

usage = "usage: proxy <from-port> <to-port>"

configFromArgs :: IO Config
configFromArgs = do
  args <- getArgs
  when (length args /= 2) $ die usage
  case (toAddr (head args), toAddr (args !! 1)) of
    (Just from, Just to) -> return (Config (Just from) to)
    _ -> die usage
  where
    toAddr portString =
      let maybePort = readMaybe portString in
        fmap (`SockAddrInet` localhost) maybePort

main :: IO ()
main = do
  configFromArgs >>= enableProxy . proxyFromConfig
  return ()

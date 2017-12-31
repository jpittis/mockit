module Main where

import Lib

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import System.Environment (getArgs)
import System.Exit (die)
import Text.Read(readMaybe)

import Control.Monad (when)

-- TODO
-- ----
-- - Need a way to turn proxy on and off.
-- - Accept buffer and 3 way handshake timeouts.
-- - Latency sounds possible and easy.
-- - Blackhole... is actually soooo sketchy but probably easy.
-- - Don't just transmit on newline.
-- - Write some tests.
-- - Provide an listen port if not privded.

usage = "usage: proxy <from-port> <to-port>"

configFromArgs :: IO Config
configFromArgs = do
  args <- getArgs
  when (not $ (length args) == 2) $ die usage
  case (toAddr (args !! 0), toAddr (args !! 1)) of
    (Just from, Just to) -> return (Just from, to)
    otherwise -> die usage
  where
    toAddr portString =
      let maybePort = readMaybe portString in
        fmap (\p -> (SockAddrInet p (tupleToHostAddress (127, 0, 0, 1)))) maybePort

main :: IO ()
main = do
  configFromArgs >>= startProxy
  return ()

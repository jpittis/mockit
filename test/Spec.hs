{-# LANGUAGE OverloadedStrings #-}

import Lib

import Test.Hspec

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Concurrent (forkIO)

import qualified Data.ByteString as BS

testAddr = (SockAddrInet aNY_PORT localhost)

startEchoServer :: IO SockAddr
startEchoServer = do
  server <- socket AF_INET Stream 0
  bind server testAddr
  listen server 1
  forkIO $ acceptOneRequest server
  getSocketName server
  where
    acceptOneRequest server = do
      (conn, _) <- accept server
      echoLoop conn
    echoLoop conn = do
      content <- recv conn 1028
      send conn content
      echoLoop conn

attemptEcho :: SockAddr -> BS.ByteString -> IO BS.ByteString
attemptEcho addr msg = do
  client <- socket AF_INET Stream 0
  connect client addr
  send client msg
  recv client 1028

assertEchoSuccess :: SockAddr -> Expectation
assertEchoSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  resp <- attemptEcho addr msg
  msg `shouldBe` resp

main :: IO ()
main = hspec $ do
  describe "Lib" $ do
    it "echos stream data" $ do
      echoAddr <- startEchoServer
      assertEchoSuccess echoAddr
    it "proxies between two connections" $ do
      echoAddr <- startEchoServer
      listenAddr <- startProxy (Nothing, echoAddr)
      assertEchoSuccess listenAddr

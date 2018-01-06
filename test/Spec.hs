{-# LANGUAGE OverloadedStrings #-}

import Lib

import Test.Hspec

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.MVar (putMVar, newEmptyMVar, readMVar)

import qualified Data.ByteString as BS
import Control.Monad (when)

import Control.Concurrent.Async (async, cancel, wait)

import Control.Exception (catch, SomeException)

testAddr = (SockAddrInet aNY_PORT localhost)

startEchoServer :: Bool -> IO SockAddr
startEchoServer acceptOne = do
  server <- socket AF_INET Stream 0
  bind server testAddr
  listen server 1
  if acceptOne then
    async $ acceptOneRequest server
  else
    async $ return ()
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

echoSuccess :: SockAddr -> IO Bool
echoSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  resp <- returnAfter 100 (attemptEcho addr msg) ""
  return (msg == resp)

returnAfter :: Int -> IO a -> a -> IO a
returnAfter millisec action onFail =
  catch (failAfter millisec action) (returnOnFail onFail)
  where
    returnOnFail :: a -> SomeException -> IO a
    returnOnFail onFail _ = return onFail

failAfter :: Int -> IO a -> IO a
failAfter millisec action = do
  a <- async action
  threadDelay (millisec * 1000)
  cancel a
  wait a

main :: IO ()
main = hspec $ do
  describe "EchoServer" $ do
    it "echos stream data" $ do
      echoAddr <- startEchoServer True
      echoSuccess echoAddr `shouldReturn` True

  describe "Lib" $ do
    it "does not have an addr when disabled" $ do
      let proxy = proxyFromConfig (Nothing, testAddr)
      proxyEnabled proxy `shouldBe` False
      proxyAddr proxy `shouldBe` Nothing

    it "proxies between two connections when enabled" $ do
      echoAddr <- startEchoServer True
      let proxy = proxyFromConfig (Nothing, echoAddr)
      proxy <- enableProxy proxy
      proxyEnabled proxy `shouldBe` True
      let addr = case proxyAddr proxy of
                   Just addr -> addr
                   Nothing -> error "proxy not enabled"
      echoSuccess echoAddr `shouldReturn` True
      proxy <- disableProxy proxy
      proxyEnabled proxy `shouldBe` False
      echoSuccess echoAddr `shouldReturn` False

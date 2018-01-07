{-# LANGUAGE OverloadedStrings #-}

import Lib

import Test.Hspec

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race)

import qualified Data.ByteString as BS
import Control.Monad (when)

main :: IO ()
main = hspec $ do
  describe "EchoServer" $
    it "echos stream data" $
      do echoAddr <- startEchoServer True
         echoSuccess echoAddr `shouldReturn` True

  describe "Lib" $ do
    it "does not have an addr when disabled" $ do
      let proxy = proxyFromConfig (Config Nothing testAddr)
      proxyEnabled proxy `shouldBe` False
      proxyAddr proxy `shouldBe` Nothing

    it "proxies between two connections when enabled" $ do
      echoAddr <- startEchoServer True
      let proxy = proxyFromConfig (Config Nothing echoAddr)
      proxy <- enableProxy proxy
      proxyEnabled proxy `shouldBe` True
      let Just addr = proxyAddr proxy
      echoSuccess echoAddr `shouldReturn` True
      proxy <- disableProxy proxy
      proxyEnabled proxy `shouldBe` False
      echoSuccess echoAddr `shouldReturn` False

testAddr = SockAddrInet aNY_PORT localhost

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
  resp <- timeoutAfter 10 (attemptEcho addr msg) (return "")
  return (msg == resp)

-- timeoutAfter returns the result of onTimeout if action does not return
-- within millisec milliseconds.
timeoutAfter :: Int -> IO a -> IO a -> IO a
timeoutAfter millisec action onTimeout = do
  result <- race action delayReturn
  return $ either id id result
  where
    delayReturn = do
      threadDelay (millisec * 1000)
      onTimeout

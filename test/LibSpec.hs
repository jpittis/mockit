{-# LANGUAGE OverloadedStrings #-}

module LibSpec (spec) where

import Lib

import Test.Hspec

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race)

import qualified Data.ByteString as BS
import Control.Monad (replicateM_)

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

spec :: Spec
spec = do
  describe "EchoServer" $
    it "echos stream data" $ do
      echoAddr <- newTestAddr
      startEchoServer echoAddr 2
      echoSuccess echoAddr `shouldReturn` True
      echoSuccess echoAddr `shouldReturn` True
      echoSuccess echoAddr `shouldReturn` False

  describe "Proxy" $ do
    it "disabled proxy does not listen" $ do
      config@(Config listenAddr _) <- uniqueConfig
      let proxy = proxyFromConfig config
      echoSuccess listenAddr `shouldThrow` anyException

    it "enabled proxy proxies and stops when disabled" $ do
      config@(Config listenAddr upstreamAddr) <- uniqueConfig
      startEchoServer upstreamAddr 1
      let proxy = proxyFromConfig config
      proxy <- enableProxy proxy
      echoSuccess listenAddr `shouldReturn` True
      proxy <- disableProxy proxy
      echoSuccess listenAddr `shouldThrow` anyException

    it "timeout proxy listens but does not accept" $ do
      config@(Config listenAddr _) <- uniqueConfig
      let proxy = proxyFromConfig config
      proxy <- timeoutProxy proxy
      echoSuccess listenAddr `shouldReturn` False
      proxy <- disableProxy proxy
      echoSuccess listenAddr `shouldThrow` anyException

    it "enabled proxy can accept multiple connections" $ do
      config@(Config listenAddr upstreamAddr) <- uniqueConfig
      startEchoServer upstreamAddr 3
      let proxy = proxyFromConfig config
      proxy <- enableProxy proxy
      client1 <- createClient listenAddr
      client2 <- createClient listenAddr
      client3 <- createClient listenAddr
      attemptEcho client1 "1" `shouldReturn` "1"
      attemptEcho client2 "2" `shouldReturn` "2"
      attemptEcho client3 "3" `shouldReturn` "3"

  describe "Resilient Proxy" $
    it "behaves correctly when upstream is not present" $ do
      config@(Config listenAddr upstreamAddr) <- uniqueConfig
      let proxy = proxyFromConfig config
      proxy <- enableProxy proxy
      echoSuccess listenAddr `shouldReturn` False
      startEchoServer upstreamAddr 1
      echoSuccess listenAddr `shouldReturn` True
      echoSuccess listenAddr `shouldReturn` False

    -- it "behaves correctly when upstream closes" $ do

    -- it "behaves correctly when downstream closes" $ do

-- We use the port range 20,000 and above for testing.
-- This is the sketchiest thing I've ever done in Haskell.
-- Mostly to spite Xavier.
testPortID :: IORef PortNumber
{-# NOINLINE testPortID #-}
testPortID = unsafePerformIO (newIORef 20000)
newTestAddr :: IO SockAddr
newTestAddr = do
  port <- readIORef testPortID
  modifyIORef testPortID (+1)
  return $ SockAddrInet port localhost

uniqueConfig :: IO Config
uniqueConfig = do
  listenAddr <- newTestAddr
  upstreamAddr <- newTestAddr
  return $ Config listenAddr upstreamAddr

localhost = tupleToHostAddress (127, 0, 0, 1)

startEchoServer :: SockAddr -> Int -> IO ()
startEchoServer listenAddr numAccepts = do
  server <- socket AF_INET Stream 0
  bind server listenAddr
  listen server 1
  async $ replicateM_ numAccepts (acceptOneRequest server)
  return ()
  where
    acceptOneRequest server = do
      (conn, _) <- accept server
      async $ echoLoop conn
    echoLoop conn = do
      content <- recv conn 1028
      send conn content
      echoLoop conn

attemptEcho :: Socket -> BS.ByteString -> IO BS.ByteString
attemptEcho client msg = do
  send client msg
  recv client 1028

createClient :: SockAddr -> IO Socket
createClient addr = do
  client <- socket AF_INET Stream 0
  connect client addr
  return client

sendSuccess :: SockAddr -> IO Bool
sendSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  timeoutAfter 100 (attemptSendSuccess msg) (return False)
  where
    attemptSendSuccess msg = do
      client <- createClient addr
      send client msg
      close client
      return True

echoSuccess :: SockAddr -> IO Bool
echoSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  client <- createClient addr
  resp <- timeoutAfter 100 (attemptEcho client msg) (return "")
  return (msg == resp)

timeoutAfter :: Int -> IO a -> IO a -> IO a
timeoutAfter millisec action onTimeout = do
  result <- race action delayReturn
  return $ either id id result
  where
    delayReturn = do
      threadDelay (millisec * 1000)
      onTimeout

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
      let proxy = proxyFromConfig (Config listenAddr upstreamAddr)
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

attemptEcho :: SockAddr -> BS.ByteString -> IO BS.ByteString
attemptEcho addr msg = do
  client <- attemptSend addr msg
  recv client 1028

attemptSend :: SockAddr -> BS.ByteString -> IO Socket
attemptSend addr msg = do
  client <- socket AF_INET Stream 0
  connect client addr
  send client msg
  return client

sendSuccess :: SockAddr -> IO Bool
sendSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  timeoutAfter 100 (attemptSendSuccess msg) (return False)
  where
    attemptSendSuccess msg = do
      client <- attemptSend addr msg
      close client
      return True

echoSuccess :: SockAddr -> IO Bool
echoSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  resp <- timeoutAfter 100 (attemptEcho addr msg) (return "")
  return (msg == resp)

timeoutAfter :: Int -> IO a -> IO a -> IO a
timeoutAfter millisec action onTimeout = do
  result <- race action delayReturn
  return $ either id id result
  where
    delayReturn = do
      threadDelay (millisec * 1000)
      onTimeout

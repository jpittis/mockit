{-# LANGUAGE OverloadedStrings #-}

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

main :: IO ()
main = hspec $ do
  describe "EchoServer" $
    it "echos stream data" $
      do echoAddr <- startEchoServer 2
         echoSuccess echoAddr `shouldReturn` True
         echoSuccess echoAddr `shouldReturn` True
         echoSuccess echoAddr `shouldReturn` False

  describe "Lib" $ do
    it "is not listening when disabled" $ do
      config <- uniqueConfig
      let proxy = proxyFromConfig config
      proxyEnabled proxy `shouldBe` False
      let listenAddr = proxyAddr proxy
      echoSuccess listenAddr `shouldThrow` anyException

    it "proxies when enabled and stops listening when disabled" $ do
      echoAddr <- startEchoServer 2
      listenAddr <- newTestAddr
      let proxy = proxyFromConfig (Config listenAddr echoAddr)
      proxy <- enableProxy proxy
      proxyEnabled proxy `shouldBe` True
      echoSuccess listenAddr `shouldReturn` True
      proxy <- disableProxy proxy
      proxyEnabled proxy `shouldBe` False
      echoSuccess listenAddr `shouldThrow` anyException

    it "causes timeouts when not accepting" $ do
      echoAddr <- startEchoServer 1
      listenAddr <- newTestAddr
      let proxy = proxyFromConfig (Config listenAddr echoAddr)
      proxy <- timeoutProxy proxy
      proxyEnabled proxy `shouldBe` True
      let addr = proxyAddr proxy
      echoSuccess listenAddr `shouldReturn` False

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

startEchoServer :: Int -> IO SockAddr
startEchoServer numAccepts = do
  server <- socket AF_INET Stream 0
  listenAddr <- newTestAddr
  bind server listenAddr
  listen server 1
  async $ replicateM_ numAccepts (acceptOneRequest server)
  getSocketName server
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

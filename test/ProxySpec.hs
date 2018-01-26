{-# LANGUAGE OverloadedStrings #-}

module ProxySpec (spec) where

import Proxy

import Test.Hspec

import Network.Socket.ByteString
import Network.Socket hiding (send, recv)

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race, withAsync, wait)

import qualified Data.ByteString as BS
import Control.Monad (replicateM_)

import System.IO.Error (catchIOError)

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
      client1 <- connectTo listenAddr
      client2 <- connectTo listenAddr
      client3 <- connectTo listenAddr
      attemptEcho client1 "1" `shouldReturn` "1"
      attemptEcho client2 "2" `shouldReturn` "2"
      attemptEcho client3 "3" `shouldReturn` "3"

  describe "Resilient Proxy" $ do
    it "behaves correctly when upstream is not present" $ do
      (listenAddr, upstreamAddr, proxy) <- createEnabledProxy
      echoSuccess listenAddr `shouldReturn` False
      startEchoServer upstreamAddr 1
      echoSuccess listenAddr `shouldReturn` True
      echoSuccess listenAddr `shouldReturn` False

    it "cleans up correctly when upstream closes" $
      ensureCleansUpAfter $ \client server proxy ->
        close server

    it "cleans up correctly when client closes" $
      ensureCleansUpAfter $ \client server proxy ->
        close client

    it "cleans up correctly when proxy is disabled" $
      ensureCleansUpAfter $ \client server proxy ->
        close client

    -- TODO I need to make sure a downstream or upstream close causes forward loop to exit.
    -- TODO I have a feeling ipv6 is breaking things again. 127.0.0.1 works but localhost does not.
    -- TODO A create, delete and create again seems to hang

ensureCleansUpAfter :: (Socket -> Socket -> Proxy -> IO ()) -> IO ()
ensureCleansUpAfter f = do
  (client, server, proxy) <- proxySocketPair
  canProxy client server `shouldReturn` True
  canProxy server client `shouldReturn` True
  f client server proxy
  canProxy client server `shouldReturn` False
  canProxy server client `shouldReturn` False

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

connectTo :: SockAddr -> IO Socket
connectTo addr = do
  client <- socket AF_INET Stream 0
  connect client addr
  return client

sendSuccess :: SockAddr -> IO Bool
sendSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  timeoutAfter 100 (attemptSendSuccess msg) (return False)
  where
    attemptSendSuccess msg = do
      client <- connectTo addr
      send client msg
      close client
      return True

echoSuccess :: SockAddr -> IO Bool
echoSuccess addr = do
  let msg = "foobar" :: BS.ByteString
  client <- connectTo addr
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

canProxy :: Socket -> Socket -> IO Bool
canProxy from to = catchIOError attemptProxy (\e -> return False)
  where
    attemptProxy = do
      let msg = "send over proxy" :: BS.ByteString
      send from msg
      result <- recv to 1024
      return $ result == msg

proxySocketPair :: IO (Socket, Socket, Proxy)
proxySocketPair = do
  (listenAddr, upstreamAddr, proxy) <- createEnabledProxy
  upstream <- listenOn upstreamAddr
  withAsync (acceptOne upstream) $ \asyncConn -> do
    client <- connectTo listenAddr
    server <- wait asyncConn
    return (client, server, proxy)
  where
    acceptOne :: Socket -> IO Socket
    acceptOne server = do
      (client, _) <- accept server
      close server
      return client
    listenOn :: SockAddr -> IO Socket
    listenOn addr = do
      server <- socket AF_INET Stream 0
      bind server addr
      listen server 1
      return server

createEnabledProxy :: IO (SockAddr, SockAddr, Proxy)
createEnabledProxy = do
  config@(Config listenAddr upstreamAddr) <- uniqueConfig
  let proxy = proxyFromConfig config
  proxy <- enableProxy proxy
  return (listenAddr, upstreamAddr, proxy)

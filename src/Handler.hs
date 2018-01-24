module Handler
    ( startHandler
    , stopHandler
    , HandlerReader
    , runCommand
    , Handler
    , handleCommand
    , HandleCommand
    ) where

import qualified Api

import Proxy
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map.Strict as Map

import Control.Concurrent.MVar
import Control.Concurrent.Async (async, Async, cancel, wait)

import Data.Text (Text)
import Data.Word (Word16)

import Control.Monad.Trans.Reader (ReaderT, ask)

import Network.Socket

import Data.Maybe (isJust)

data Handler s = Handler {
    handlerRequests :: MVar Request
  , handlerHandle   :: Async s
  , handlerShutdown :: MVar ()
}

data Request = Request { reqCommand :: Api.Command, reqResponse :: MVar Api.Response }

type HandlerReader s = ReaderT (Handler s) IO

type HandlerState s a = StateT s IO a

type HandleCommand s = Api.Command -> HandlerState s Api.Response

type ProxyMap = Map.Map Text Proxy
type Proxies a = HandlerState ProxyMap a

startHandler :: s -> HandleCommand s -> IO (Handler s)
startHandler init h = do
  requests <- newEmptyMVar
  hShutdown <- newEmptyMVar
  handle <- async $ requestLoop requests init hShutdown
  return (Handler requests handle hShutdown)
  where
    requestLoop requests state hShutdown = do
      (Request command resps) <- takeMVar requests
      (r, s) <- runStateT (h command) state
      putMVar resps r
      maybeShutdown <- tryTakeMVar hShutdown
      if isJust maybeShutdown then
        return s
      else
        requestLoop requests s hShutdown

stopHandler :: Handler s -> IO s
stopHandler (Handler requests handle hShutdown) = do
  sendShutdownSignal
  unblockHandler
  wait handle
  where
    sendShutdownSignal =
      putMVar hShutdown ()
    unblockHandler = do
      resp <- newEmptyMVar
      putMVar requests (Request Api.List resp)

runCommand :: Api.Command -> (HandlerReader s) Api.Response
runCommand command = do
  (Handler requests _ _) <- ask
  response <- liftIO newEmptyMVar
  liftIO $ putMVar requests (Request command response)
  liftIO $ takeMVar response

handleCommand :: Api.Command -> Proxies Api.Response

handleCommand (Api.Create name listenHost listenPort upstreamHost upstreamPort) = do
  config <- liftIO $ createConfig listenHost listenPort upstreamHost upstreamPort
  createProxy name config
  where
    createProxy name config = do
      proxies <- get
      let proxy = proxyFromConfig config
      proxy <- liftIO $ enableProxy proxy
      put $ Map.insert name proxy proxies
      return $ Api.SuccessResp True
    createConfig listenHost listenPort upstreamHost upstreamPort = do
      listenAddr <- getAddr listenHost listenPort
      upstreamAddr <- getAddr upstreamHost upstreamPort
      return $ Config listenAddr upstreamAddr
    getAddr :: HostName -> Word16 -> IO SockAddr
    getAddr host port = do
      let hints = defaultHints { addrFlags = [AI_NUMERICSERV]
                               , addrFamily = AF_INET
                               , addrSocketType = Stream }
      addr : _ <- getAddrInfo (Just hints) (Just host) (Just (show port))
      return $ addrAddress addr

handleCommand (Api.Delete name) = do
  proxies <- get
  put $ Map.delete name proxies
  return $ Api.SuccessResp True

handleCommand (Api.Update name state) = do
  proxy <- findProxy name
  case proxy of
    Just proxy -> updateState proxy
    Nothing -> return $ Api.SuccessResp False
  where
    updateState proxy = do
      proxy <- liftIO $ update state proxy
      proxies <- get
      put $ Map.insert name proxy proxies
      return $ Api.SuccessResp True
    update Api.Disabled = disableProxy
    update Api.Enabled  = enableProxy
    update Api.Timeout  = timeoutProxy

handleCommand (Api.Get name) = do
  proxy <- findProxy name
  case proxy of
    Just proxy -> do
      proxy <- liftIO $ proxyToApi (name, proxy)
      return $ Api.ProxyResp proxy
    Nothing -> return $ Api.SuccessResp False

handleCommand Api.List = do
  proxies <- get
  proxies <- liftIO $ mapM proxyToApi (Map.assocs proxies)
  return $ Api.ProxiesResp proxies

findProxy :: Text -> Proxies (Maybe Proxy)
findProxy name = do
  proxies <- get
  return $ Map.lookup name proxies

proxyToApi :: (Text, Proxy) -> IO Api.Proxy
proxyToApi (name, proxy) = do
  let (Config listenAddr upstreamAddr) = proxyConfig proxy
  (Just listenHost, Just listenService) <- getNameInfo [NI_NUMERICSERV] True True listenAddr
  (Just upstreamHost, Just upstreamService) <- getNameInfo [NI_NUMERICSERV] True True upstreamAddr
  return $ Api.Proxy name (proxyApiState proxy) listenHost listenService upstreamHost upstreamService

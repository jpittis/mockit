{-# LANGUAGE OverloadedStrings #-}

module ExampleSpec (spec) where

import Api
import Client
import Handler
import Http

import Test.Hspec

import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Network.HTTP.Req
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Default.Class

import qualified Data.Map.Strict as Map (empty)
import Data.Text (Text)

import Control.Exception
import System.IO.Error

import Data.Default.Class (def)
import Network.Wai.Handler.Warp (setPort)

startServer =
  scottyOpts opts (get "/" $ status status200)
  where
    opts = def { verbose = 0
               , settings = setPort 5000 $ settings def
               }

data ReqResponse = TimeoutR | Success | Failure | Exception
  deriving (Show, Eq)

reqSuccess :: Int -> IO ReqResponse
reqSuccess p = do
  e  <- try raceReq :: IO (Either SomeException ReqResponse)
  case e of
    Right response -> return response
    Left _ -> return Exception
  where
    raceReq = do
      e <- race sendReq (threadDelay (10^6) >> return TimeoutR)
      case e of
        Right r -> return r
        Left r -> return r
    sendReq = do
      resp <- runReq def $
        req GET (http "localhost") NoReqBody ignoreResponse (port p)
      if responseStatusCode resp == 200 then
        return Success
      else
        return Failure

spec :: Spec
spec =
  describe "ExampleServer" $ do
    it "responds with success without proxy" $ do
      handle <- async startServer
      reqSuccess 5000 `shouldReturn` Success
      cancel handle

    it "attempts all proxy states" $ do
      -- This is effectively what's run when you execute the mockit-server binary. It's going to
      -- create an HTTP server listening on port 3000 whose API you will use to create an manipulate
      -- proxies.
      handler <- startHandler Map.empty handleCommand
      server <- async $ serve handler

      -- We're also going to use startServer to create an example HTTP service on port 5000. It
      -- always returns success. We're going to be testing a proxy in between this test and this
      -- example service.
      withAsync startServer $ \_ -> do
        -- Let's start by creating a proxy. It's going to listen on port 4000 and forward to our
        -- example HTTP service on port 5000.
        resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
        resp `shouldBe` SuccessResp True
        -- We can assert that the proxies properly forwards the HTTP request.
        reqSuccess 4000 `shouldReturn` Success

        -- Now we can ensure that disabling the proxy will cause HTTP requests through it to raise
        -- exceptions because there isn't a socket listening on that port.
        resp <- sendCommand $ Update "example" Disabled
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Exception

        -- The timeout state won't raise an exception because the's a socket listening. But the
        -- socket isn't accepting connections which means the HTTP request should timeout.
        resp <- sendCommand $ Update "example" Timeout
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` TimeoutR

        -- Finally, we can move the proxy back into an enabled state and the request should be
        -- successful yet again.
        resp <- sendCommand $ Update "example" Enabled
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        resp <- sendCommand $ Update "example" Disabled
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Exception

        stopHandler handler
        cancel server

    it "stopping the handler cleans up the sub processes" $ do
      handler <- startHandler Map.empty handleCommand
      server <- async $ serve handler
      withAsync startServer $ \_ -> do
        resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        resp <- sendCommand $ Delete "example"
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Exception

        resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        stopHandler handler
        cancel server

      -- Make sure stop handler cleans up the proxy so we can create it again.
      handler <- startHandler Map.empty handleCommand
      server <- async $ serve handler
      withAsync startServer $ \_ -> do
        resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        stopHandler handler
        cancel server


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

import Control.Exception (try, SomeException)

startServer = scotty 5000 (get "/" $ status status200)

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
      e <- race sendReq (threadDelay (5^6) >> return TimeoutR)
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
    it "responds with success without proxy" $
      withAsync startServer $ \_ ->
        reqSuccess 5000 `shouldReturn` Success

    it "attempts all proxy states" $ do
      handler <- startHandler Map.empty handleCommand
      server <- async $ serve handler
      withAsync startServer $ \_ -> do
        resp <- sendCommand $ Create "example" "localhost" 4000 "localhost" 5000
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        resp <- sendCommand $ Update "example" Disabled
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Exception

        resp <- sendCommand $ Update "example" Timeout
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` TimeoutR

        resp <- sendCommand $ Update "example" Enabled
        resp `shouldBe` SuccessResp True
        reqSuccess 4000 `shouldReturn` Success

        stopHandler handler
        cancel server


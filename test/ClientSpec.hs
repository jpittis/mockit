{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import Api
import Handler
import Client
import Http

import Test.Hspec

import Control.Monad.Trans.State.Lazy
import Control.Concurrent.Async

import Control.Monad (mapM_)

import Data.Text (Text)

spec :: Spec
spec =
  describe "Client" $
    it "requests all the endpoints" $ do
      handler <- startHandler [] handleCommandList
      server <- async $ serve handler
      let commands = [ List
                     , Get "foo"
                     , Delete "foo"
                     , Create ("foo" :: Text) "localhost" 3333 "localhost" 4444
                     , Update "foo" Timeout
                     ]
      mapM_ sendCommand commands
      finalState <- stopHandler handler
      cancel server
      finalState `shouldBe` (List : reverse commands)

handleCommandList :: HandleCommand [Command]
handleCommandList command = do
  commands <- get
  put $ command : commands
  return $ SuccessResp True

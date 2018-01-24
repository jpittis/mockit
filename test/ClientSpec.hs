module ClientSpec (spec) where

import Api
import Handler
import Client
import Http

import Test.Hspec

import Control.Monad.Trans.State.Lazy
import Control.Concurrent.Async

spec :: Spec
spec =
  describe "Client" $
    it "requests all the endpoints" $ do
      handler <- startHandler [] handleCommandList
      server <- async $ serve handler
      resp <- sendCommand List
      print resp
      resp <- sendCommand List
      print resp
      stopHandler handler
      cancel server
      [] `shouldBe` [List, List, List]

handleCommandList :: HandleCommand [Command]
handleCommandList command = do
  commands <- get
  put $ command : commands
  return $ SuccessResp True

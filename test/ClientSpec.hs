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
      handler <- startHandler :: IO MockHandler
      server <- async $ serve handler
      -- (_, MockHandler commands) <- runCommands handler $ do
      sendCommand List
      sendCommand List
      sendCommand List
      stopHandler handler
      cancel server
      commands `shouldBe` [List, List, List]

-- startServer =
--   handler <- startHandler :: IO MockHandler

newtype MockHandler = MockHandler { parsedCommands :: [Command] }

instance Handler MockHandler where
  startHandler = return (MockHandler [])
  stopHandler _ = return ()
  runCommand command = do
    (MockHandler parsedCommands) <- get
    let commands = command : parsedCommands
    put (MockHandler commands)
    return $ SuccessResp True

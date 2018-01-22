{-# LANGUAGE OverloadedStrings #-}

module OrchestratorSpec (spec) where

import Api
import Orchestrator

import Test.Hspec

import Control.Monad.Trans.Reader (withReaderT, runReaderT)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)

spec :: Spec
spec =
  describe "Orchestrator" $
    it "starts with no proxies" $ do
      orch <- startOrchestrator
      runCommands orch $ do
        resp <- runCommand List
        liftIO $ resp `shouldBe` ProxiesResp []
        resp2 <- runCommand (Create ("foo" :: Text) "localhost" 3333 "localhost" 4444)
        liftIO $ print "ran command"
       --  liftIO $ resp2 `shouldBe` ProxiesResp []
        -- liftIO $ print "all done"
      stopOrchestrator orch

runCommands :: Orch -> OrchReader a -> IO a
runCommands orch action =
  runReaderT action orch

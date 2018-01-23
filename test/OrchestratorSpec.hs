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
    it "can handle all commands" $ do
      orch <- startOrchestrator
      runCommands orch $ do
        resp <- runCommand List
        liftIO $ resp `shouldBe` ProxiesResp []

        resp <- runCommand (Create ("foo" :: Text) "localhost" 3333 "127.0.0.1" 4444)
        liftIO $ resp `shouldBe` SuccessResp True

        resp <- runCommand List
        let foo = Proxy { proxyName = "foo"
                        , proxyState = Enabled
                        , proxyListenHost = "localhost"
                        , proxyListenPort = "3333"
                        , proxyUpstreamHost = "localhost"
                        , proxyUpstreamPort = "4444" }
        liftIO $ resp `shouldBe` ProxiesResp [foo]

        resp <- runCommand (Create ("bar" :: Text) "localhost" 5555 "127.0.0.1" 6666)
        liftIO $ resp `shouldBe` SuccessResp True

        resp <- runCommand List
        let bar = Proxy { proxyName = "bar"
                        , proxyState = Enabled
                        , proxyListenHost = "localhost"
                        , proxyListenPort = "5555"
                        , proxyUpstreamHost = "localhost"
                        , proxyUpstreamPort = "6666" }
        liftIO $ resp `shouldBe` ProxiesResp [bar, foo]

        resp <- runCommand (Update ("foo" :: Text) Disabled)
        liftIO $ resp `shouldBe` SuccessResp True
        let foo1 = foo { proxyState = Disabled }
        resp <- runCommand (Get ("foo" :: Text))
        liftIO $ resp `shouldBe` ProxyResp foo1

        resp <- runCommand (Get ("bar" :: Text))
        liftIO $ resp `shouldBe` ProxyResp bar

        resp <- runCommand (Update ("bar" :: Text) Timeout)
        liftIO $ resp `shouldBe` SuccessResp True
        let bar1 = bar { proxyState = Timeout }
        resp <- runCommand (Get ("bar" :: Text))
        liftIO $ resp `shouldBe` ProxyResp bar1

        resp <- runCommand (Delete ("foo" :: Text))
        liftIO $ resp `shouldBe` SuccessResp True

        resp <- runCommand List
        liftIO $ resp `shouldBe` ProxiesResp [bar1]

        resp <- runCommand (Delete ("bar" :: Text))
        liftIO $ resp `shouldBe` SuccessResp True

        resp <- runCommand List
        liftIO $ resp `shouldBe` ProxiesResp []
      stopOrchestrator orch

runCommands :: Orch -> OrchReader a -> IO a
runCommands orch action =
  runReaderT action orch

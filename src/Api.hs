{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
    ( listProxies
    , createProxy
    , getProxy
    , deleteProxy
    , updateProxy
    , Create
    , Delete
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Data.Text
import Data.Word (Word16)
import Network.Socket (HostName)

data State = Disabled | Enabled | Timeout
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Proxy = Proxy {
    proxyName          :: Text
  , proxyState         :: State
  , proxyListenHost    :: HostName
  , proxyListenPort    :: Word16
  , proxyUpstreamHost  :: HostName
  , proxyUpstreamPort  :: Word16
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Create = Create {
    createName         :: Text
  , createListenHost   :: HostName
  , createListenPort   :: Word16
  , createUpstreamHost :: HostName
  , createUpstreamPort :: Word16
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Delete = Delete { deleteName :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Update = Update { changeName :: Text, changeState :: State }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Get = Get { getName :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Response a = Response (Either Text a)
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

success val = Response (Right val)
failure msg = Response (Left msg)

listProxies :: Response [Proxy]
listProxies = success []

createProxy :: Create -> Response ()
createProxy (Create name listenHost listenPort upstreamHost upstreamPort) = success ()

getProxy :: Get -> Response Proxy
getProxy (Get name) = failure ("foo" :: Text)

deleteProxy :: Delete -> Response ()
deleteProxy (Delete name) = success ()

updateProxy :: Update -> Response ()
updateProxy (Update name state) = success ()

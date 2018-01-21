{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Api
    ( Create(..)
    , Delete(..)
    , Update(..)
    , Get(..)
    , Response(..)
    , Command(..)
    , State(..)
    , Proxy(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import Data.Text (Text)
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

data Command =
    CreateComm Create
  | DeleteComm Delete
  | UpdateComm Update
  | GetComm    Get
  | ListComm
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Response =
    SuccessResp Bool
  | ProxyResp Proxy
  | ProxiesResp [Proxy]

-- TODO: These String types are super duper sketch. It was because port and
-- address parsing was easier. This should change.
data Create = Create {
    createName         :: Text
  , createListenHost   :: String
  , createListenPort   :: String
  , createUpstreamHost :: String
  , createUpstreamPort :: String
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Delete = Delete { deleteName :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Update = Update { changeName :: Text, changeState :: State }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Get = Get { getName :: Text }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

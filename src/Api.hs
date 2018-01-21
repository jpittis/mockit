{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Api
    ( Command(..)
    , Response(..)
    , State(..)
    , Proxy(..)
    ) where

import Data.Aeson
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

-- TODO: These String types are super duper sketch. It was because port and
-- address parsing was easier. This should change.
data Command =
    Create {
      creatName         :: Text
    , creatListenHost   :: String
    , creatListenPort   :: String
    , creatUpstreamHost :: String
    , creatUpstreamPort :: String
           }
  | Delete { deleteName :: Text }
  | Update { updateName :: Text, updateState :: State }
  | Get    { getName :: Text }
  | List
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON Command where
  toJSON = genericToJSON $ defaultOptions { sumEncoding = UntaggedValue }

data Response =
    SuccessResp Bool
  | ProxyResp Proxy
  | ProxiesResp [Proxy]
  deriving (Show, Eq, Generic, FromJSON)

instance ToJSON Response where
  toJSON = genericToJSON $ defaultOptions { sumEncoding = UntaggedValue }

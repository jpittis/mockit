{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

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
import Network.Socket (HostName, ServiceName)

data State = Disabled | Enabled | Timeout
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Proxy = Proxy {
    proxyName          :: Text
  , proxyState         :: State
  , proxyListenHost    :: HostName
  , proxyListenPort    :: ServiceName , proxyUpstreamHost  :: HostName
  , proxyUpstreamPort  :: ServiceName
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Command =
    Create {
      creatName         :: Text
    , creatListenHost   :: HostName
    , creatListenPort   :: Word16
    , creatUpstreamHost :: HostName
    , creatUpstreamPort :: Word16
           }
  | Delete { deleteName :: Text }
  | Update { updateName :: Text, updateState :: State }
  | Get    { getName :: Text }
  | List
  deriving (Show, Eq, Generic)

data Response =
    SuccessResp { respSuccess :: Bool    }
  | ProxyResp   { respProxy   :: Proxy   }
  | ProxiesResp { respProxies :: [Proxy] }
  deriving (Show, Eq, Generic)

untagged :: Options
untagged = defaultOptions { sumEncoding = UntaggedValue }

instance ToJSON Command where
  toJSON = genericToJSON untagged

instance FromJSON Command where
  parseJSON = genericParseJSON untagged

instance ToJSON Response where
  toJSON = genericToJSON untagged

instance FromJSON Response where
  parseJSON = genericParseJSON untagged

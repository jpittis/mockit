{-# LANGUAGE OverloadedStrings #-}

module Client
    (
    ) where

import Api

import Network.HTTP.Req
import Data.Aeson
import Data.Default.Class

sendCommand :: Api.Command -> IO Api.Response
sendCommand command =
  case command of
    List ->
      send GET (https "localhost" /: "proxies") NoReqBody
    create@Create{} ->
      send POST (https "localhost" /: "proxies") (ReqBodyJson create)
    Get name ->
      send GET (https "localhost" /: "proxies") NoReqBody
    Delete name ->
      send DELETE (https "localhost" /: "proxies") NoReqBody
    update@(Update name _) ->
      send POST (https "localhost" /: "proxies") (ReqBodyJson update)
  where
    send method scheme body = do
      resp <- runReq def $
        req method scheme body jsonResponse mempty
      return $ responseBody resp


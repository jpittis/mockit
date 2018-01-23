{-# LANGUAGE OverloadedStrings #-}

module Client
    ( sendCommand
    ) where

import Api

import Network.HTTP.Req
import Data.Aeson
import Data.Default.Class

sendCommand :: Api.Command -> IO Api.Response
sendCommand command =
  case command of
    List ->
      send GET (http "localhost:3000" /: "proxies") NoReqBody
    create@Create{} ->
      send POST (http "localhost:3000" /: "proxies") (ReqBodyJson create)
    Get name ->
      send GET (http "localhost:3000" /: "proxies" /: name) NoReqBody
    Delete name ->
      send DELETE (http "localhost:3000" /: "proxies" /: name) NoReqBody
    update@(Update name _) ->
      send POST (http "localhost:3000" /: "proxies" /: name) (ReqBodyJson update)
  where
    send method scheme body = do
      resp <- runReq def $
        req method scheme body jsonResponse mempty
      return $ responseBody resp

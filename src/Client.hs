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
      send GET (http "localhost" /: "proxies") NoReqBody
    create@Create{} ->
      send POST (http "localhost" /: "proxies") (ReqBodyJson create)
    Get name ->
      send GET (http "localhost" /: "proxies" /: name) NoReqBody
    Delete name ->
      send DELETE (http "localhost" /: "proxies" /: name) NoReqBody
    update@Update{} ->
      send PUT (http "localhost" /: "proxies") (ReqBodyJson update)
  where
    send method scheme body = do
      resp <- runReq def $
        req method scheme body jsonResponse (port 3000)
      return $ responseBody resp

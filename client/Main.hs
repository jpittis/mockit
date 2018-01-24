{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api (Command, State)
import Client (sendCommand)

import Options.Generic

import Data.List (findIndex)
import Data.Char (isUpper, toLower)

import Network.HTTP.Req (HttpException(..))
import Control.Exception (catch)
import System.Exit (die)

instance ParseRecord State
instance ParseField State
instance ParseFields State
instance ParseRecord Command where
  parseRecord = parseRecordWithModifiers $
    defaultModifiers { fieldNameModifier = stripFirstWord }

stripFirstWord :: String -> String
stripFirstWord word =
  let (Just front)    = findIndex isUpper word
      (first : rest ) = drop front word in
      toLower first : rest

main = do
  command <- getRecord "mockit-client"
  resp <- sendCommand command
  print resp


{-# language OverloadedStrings #-}
module Main where

import Lib
import qualified Data.Text.Lazy as T
-- import qualified Data.ByteString.Lazy as BS
-- import qualified Data.Vector as V
-- import qualified Data.Aeson as J

import Web.Scotty (scotty, ScottyM, get, post, param, text, json, jsonData)

main :: IO ()
main = scotty 3000 $ do
  oneShot
  batchProcess

-- | Classify a single datum, provided as parameters to a GET endpoint
oneShot :: ScottyM ()
oneShot = get "/model/v1/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let v = V2 x y
      c = clf0 v     
  text $ T.pack $ show c

-- | Classify a batch of data, encoded in the JSON body of a POST request
batchProcess :: ScottyM ()
batchProcess = post "/model/v1/batch" $ do
  js <- jsonData
  let rs = classifyBatchWith clf0 js
  json rs

-- | Binary classifier with hardcoded parameters
clf0 :: V2 Double -> Bool
clf0 = classify coeffs0

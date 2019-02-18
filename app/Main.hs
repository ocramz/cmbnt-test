{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import Lib
import Lib.Math

import Control.Applicative
import qualified Data.Text.Lazy as T
-- import qualified Data.ByteString.Lazy as BS
-- import qualified Data.Vector as V
-- import qualified Data.Aeson as J
import Network.HTTP.Types.Status (status200)
import Web.Scotty (scottyOpts, Options(..), ScottyM, get, post, param, html, text, json, jsonData, status, raise)

import Network.Wai.Handler.Warp (defaultSettings)
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)



main :: IO ()
main = scottyOpts (Options { verbose = 0, settings = defaultSettings}) $ do 
  hello
  liveness
  oneShot
  batchProcess

-- | Classify a single datum, provided as parameters to a GET endpoint
oneShot :: ScottyM ()
oneShot = get "/model/v1/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let v = mkV2 x y
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


-- | Send a labeled batch of data to the server for training
-- train = post "/model/v2/train" $ do
--   js <- jsonData
  


-- | Liveness endpoint
liveness :: ScottyM ()
liveness = get "/liveness" $ status status200

-- | Homepage
hello :: ScottyM ()
hello = get "/" $ do
  status status200
  html "<h1>Hello! </h1>"

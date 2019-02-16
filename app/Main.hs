{-# language OverloadedStrings #-}
module Main where

import Lib 
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

-- import Control.Monad.IO.Class

import Web.Scotty

-- import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $ do
  oneShot
  batchProcess

oneShot :: ScottyM ()
oneShot = get "/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let v = V2 x y
      c = classify coeffs0 v     
  text $ T.pack $ show c

batchProcess :: ScottyM ()
batchProcess = post "/batch" $ do
  js <- jsonData
  let rs = maybe [] (map (classify coeffs0) . batch) js
  text $ T.pack $ show rs

  

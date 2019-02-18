{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS

import Control.Applicative (Alternative(..))

import Control.Concurrent.STM (STM(..), TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Monad.Reader (ReaderT(..), runReaderT, MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))

import Web.Scotty.Trans (Options(..), ScottyT, scottyT, scottyOptsT, middleware, get, text, post, status, html, jsonData, param, json)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.HTTP.Types.Status (status200)

import qualified Data.Vector as V

import Lib
import Lib.Math

main :: IO ()
main = do
  c0 <- newTVarIO classifierConfigDefault
  let opts = Options 0 defaultSettings
  scottyOptsT opts (runApp c0) app2

app2 :: ScottyT T.Text App ()
app2 = do
  middleware logStdoutDev
  liveness
  hello
  trainClassifier
  classifyCurrent
  classifyBatchCurrent
  classifyDefault
  classifyBatchDefault
  showInternalConfig




-- | Liveness endpoint. Replies with 200 OK when queried
liveness :: ScottyT T.Text App ()
liveness = get "/liveness" $ status status200

-- | Root endpoint ("homepage")
hello :: ScottyT T.Text App ()
hello = get "/" $ do
  status status200
  html "<h1>Hello! </h1>"

-- | Configure and train a classifier
trainClassifier :: ScottyT T.Text App ()
trainClassifier = post "/model/v2/train/" $ do
  trainConfig <- jsonData
  app $ modify $ const trainConfig

-- | Classify a single point, with the current configuration 
classifyCurrent :: ScottyT T.Text App ()
classifyCurrent = get "/model/v2/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let vx = mkV2 x y
  cconf <- app getConfig
  let classf = train cconf
  text $ T.pack $ show $ classf vx

-- | Classify a batch of points, with the current configuration
classifyBatchCurrent :: ScottyT T.Text App ()
classifyBatchCurrent = post "/model/v2/batch/" $ do
  js <- jsonData
  cconf <- app getConfig
  let classf = train cconf
      rs = classifyBatchWith classf js
  json rs

-- | Classify a single datum with the default affine classifier, provided as parameters to a GET endpoint
--
-- Uses the default classifier parameters
classifyDefault :: ScottyT T.Text App ()
classifyDefault = get "/model/v1/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let v = mkV2 x y
      c = clf0 v     
  text $ T.pack $ show c

-- | Binary classifier with hardcoded parameters
clf0 :: V2 Double -> Bool
clf0 = classify coeffs0

-- | Classify a batch of data, encoded in the JSON body of a POST request
classifyBatchDefault :: ScottyT T.Text App ()
classifyBatchDefault = post "/model/v1/batch" $ do
  js <- jsonData
  let rs = classifyBatchWith clf0 js
  json rs


showInternalConfig :: ScottyT T.Text App ()
showInternalConfig = get "/internal/" $ do
  ss <- liftIO samples
  json $ ClassifierConfig ss FDA

samples :: IO [Sample]
samples = do
  ss <- BS.readFile "data/samples.csv"
  pure $ V.toList $ either (const V.empty) id $ decodeSamples ss






-- | The type of our web application
newtype App a = App {
  unApp :: ReaderT (TVar ClassifierConfig) IO a
  } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadReader (TVar ClassifierConfig))

-- | Configure and run the 'App' in IO 
runApp :: TVar ClassifierConfig -> App a -> IO a
runApp c0 a = runReaderT (unApp a) c0

-- | Scotty monads are layered on top of our custom monad.
--
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'App' layer.
app :: MonadTrans mt => App a -> mt App a
app = lift

-- | Access part of the configuration variable
-- (compare with 'Control.Monad.Reader.asks')
gets :: (ClassifierConfig -> b) -> App b
gets f = do
  r <- ask
  x <- liftIO $ readTVarIO r
  pure $ f x

-- | Access the current internal state
getConfig :: App ClassifierConfig
getConfig = do
  r <- ask 
  liftIO $ readTVarIO r

-- | Update the configuration variable
-- (compare with 'Control.Monad.State.modify')
modify :: (ClassifierConfig -> ClassifierConfig) -> App ()
modify f = do
  r <- ask
  liftIO $ atomically $ modifyTVar' r f



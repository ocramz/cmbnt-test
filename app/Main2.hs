{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
module Main where

import qualified Data.Text.Lazy as T

import Control.Applicative (Alternative(..))

import Control.Concurrent.STM (STM(..), TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Monad.Reader (ReaderT(..), runReaderT, MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))

import Web.Scotty.Trans (Options(..), ScottyT(..), scottyT, scottyOptsT, middleware, get, text, post, status, html, jsonData, param, json)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Network.HTTP.Types.Status (status200)

import Lib
import Lib.Math

-- a main loop with state handling
main :: IO ()
main = do
  c0 <- newTVarIO classifierConfigDefault
  let opts = Options 0 defaultSettings
  scottyOptsT opts (runApp c0) app2

app2 :: ScottyT T.Text App ()
app2 = do
  middleware logStdoutDev
  get "/" $ do
    cl <- app $ gets clcClassifier
    text $ T.pack $ show cl


-- -- | Classify a single datum, provided as parameters to a GET endpoint
-- oneShot :: ScottyT T.Text App ()
-- oneShot = get "/model/v2/one-shot/:x:y" $ do
--   x <- param "x"
--   y <- param "y"
--   let v = mkV2 x y
--   text "moo"
  --     c = clf0 v     
  -- text $ T.pack $ show c

-- -- | Classify a batch of data, encoded in the JSON body of a POST request
-- batchProcess :: ScottyT T.Text App ()
-- batchProcess = post "/model/v1/batch" $ do
--   js <- jsonData
--   let rs = classifyBatchWith clf0 js
--   json rs


liveness :: ScottyT T.Text App ()
liveness = get "/liveness" $ status status200

hello :: ScottyT T.Text App ()
hello = get "/" $ do
  status status200
  html "<h1>Hello! </h1>"



-- | Our web application type
newtype App a = App {
  unApp :: ReaderT (TVar ClassifierConfig) IO a
  } deriving (Functor, Applicative, Alternative, Monad, MonadIO, MonadReader (TVar ClassifierConfig))

-- | Configure and run the 'App' in IO 
runApp :: TVar ClassifierConfig -> App a -> IO a
runApp c0 a = runReaderT (unApp a) c0

-- |Scotty monads are layered on top of our custom monad.
--
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'App' layer.
app :: MonadTrans mt => App a -> mt App a
app = lift

-- | Access part of the configuration variable
gets :: (ClassifierConfig -> b) -> App b
gets f = do
  r <- ask
  x <- liftIO $ readTVarIO r
  pure $ f x

-- | Update the configuration variable (compare with 'Control.Monad.State.modify')
modify :: (ClassifierConfig -> ClassifierConfig) -> App ()
modify f = do
  r <- ask
  liftIO $ atomically $ modifyTVar' r f



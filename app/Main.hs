{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DeriveGeneric #-}
module Main where

import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS

import Control.Applicative (Alternative(..))
import GHC.Generics

import qualified Options.Applicative as O (Parser, ParserInfo, execParser, info, helper, fullDesc, progDesc, header, strOption, metavar, help, long, short, showDefault, value)
import Options.Applicative ((<**>))
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import Control.Monad.Reader (ReaderT(..), runReaderT, MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))

import Web.Scotty.Trans (Options(..), ScottyT, scottyOptsT, middleware, get, text, post, status, html, jsonData, param, json)
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types.Status (status200)

import qualified Data.Aeson as A
import Lucid (Html(..), renderText)
import Lucid.VegaLite (mkVegaHtml)
import RigelViz

import Lib
import Lib.Math

-- | Command line options
newtype CLIOpts = CO {
  coDatasetPath :: String  -- ^ Path of the training dataset
  }

cliOptsParser :: O.Parser CLIOpts
cliOptsParser = CO <$> O.strOption (
  O.long "dataset-path" <>
  O.short 'd' <>
  O.metavar "PATH" <>
  O.help "Path of the default training dataset" <>
  O.showDefault <>
  O.value "data/samples.csv"
  )
  
cliopts :: O.ParserInfo CLIOpts
cliopts = O.info (cliOptsParser <**> O.helper) (O.fullDesc <> O.progDesc "pred-serv - a little prediction server")

-- | MAIN
--
-- 1) Parse command line options
--
-- 2) Load samples.csv from specified path
--
-- 3) Configure the classifier with default options
--
-- 4) Configure the webserver with default options and run the REST application
main :: IO ()
main = do
  (CO dpath) <- O.execParser cliopts 
  sxs <- samples dpath
  c0 <- newTVarIO $ classifierConfigDefault { clcTrainingSet = sxs }
  let sopts = Options 0 defaultSettings
  scottyOptsT sopts (runApp c0) application



application :: ScottyT T.Text App ()
application = do
  middleware logStdoutDev
  showCurrentConfig  
  liveness
  hello
  trainClassifier
  classifyCurrent
  classifyBatchCurrent
  plotDistance
  -- classifyDefault
  -- classifyBatchDefault



-- | Reply with a JSON containing the current configuration (training dataset and classification method)
showCurrentConfig :: ScottyT T.Text App ()
showCurrentConfig = get "/current-config/" $ do
  ss <- app getConfig
  json ss

-- | Liveness endpoint. Replies with 200 OK when queried
liveness :: ScottyT T.Text App ()
liveness = get "/liveness" $ status status200

-- | Root endpoint ("homepage")
--
-- @GET /@
hello :: ScottyT T.Text App ()
hello = get "/" $ do
  status status200
  html "<h1>Hello!</h1>"

-- | Configure and train a classifier
--
-- @POST /model/v2/train@
trainClassifier :: ScottyT T.Text App ()
trainClassifier = post "/model/v2/train/" $ do
  trainConfig <- jsonData
  app $ modify $ const trainConfig

-- | Classify a single point, with the current configuration
--
-- @GET /model/v2/one-shot/?x=<x>&y=<y>@
classifyCurrent :: ScottyT T.Text App ()
classifyCurrent = get "/model/v2/one-shot/:x:y" $ do
  x <- param "x"
  y <- param "y"
  let vx = mkV2 x y
  cconf <- app getConfig
  let classf = train cconf
  text $ T.pack $ show $ classf vx

-- | Classify a batch of points, with the current configuration
--
-- @POST /model/v2/batch/@
classifyBatchCurrent :: ScottyT T.Text App ()
classifyBatchCurrent = post "/model/v2/batch/" $ do
  js <- jsonData
  cconf <- app getConfig
  let classf = train cconf
      rs = classifyBatchWith classf js
  json rs

plotDistance :: ScottyT T.Text App () 
plotDistance = get "/model/v2/plot/:dx:dy:xmin:xmax:ymin:ymax" $ do
  dx <- param "dx"
  dy <- param "dy"
  xmin <- param "xmin"
  xmax <- param "xmax"
  ymin <- param "ymin"
  ymax <- param "ymax"
  cconf <- app getConfig
  let ds = [V3 (v2x v) (v2y v) (distanceMin cconf v) | v <- datagrid dx dy xmin xmax ymin ymax]
      vls = heatmap ds
  html $ renderText $ mkVegaHtml $ A.toJSON vls

heatmap :: [a] -> VLSpec a
heatmap ds = vegaLiteSpec 400 400 [
  layer MRect (DataJSON ds) $
      posEnc X "v3x" Ordinal <>
      posEnc Y "v3y" Ordinal  <>
      colourEnc "v3z" Quantitative
  ]

data V3 a = V3 { v3x :: a, v3y ::  a, v3z :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (V3 a) where

-- | grid of domain points
datagrid :: (Num a, Enum a) => a -> a -> a -> a -> a -> a -> [V2 a]
datagrid dx dy xmin xmax ymin ymax = [mkV2 x y | x <- xs, y <- ys] where
  xs = [xmin, xmin + dx .. xmax - dx]
  ys = [ymin, ymin + dy .. ymax - dy]



-- -- | Classify a single datum with the default affine classifier, provided as parameters to a GET endpoint
-- --
-- -- Uses the default classifier parameters
-- --
-- -- @GET /model/v1/one-shot/?x=<x>&y=<y>@
-- classifyDefault :: ScottyT T.Text App ()
-- classifyDefault = get "/model/v1/one-shot/:x:y" $ do
--   x <- param "x"
--   y <- param "y"
--   let v = mkV2 x y
--       c = clf0 v     
--   text $ T.pack $ show c

-- -- | Default binary classifier with hardcoded parameters
-- clf0 :: V2 Double -> Bool
-- clf0 = classify coeffs0

-- -- | Classify a batch of data, encoded in the JSON body of a POST request
-- --
-- -- @POST /model/v1/batch/@
-- classifyBatchDefault :: ScottyT T.Text App ()
-- classifyBatchDefault = post "/model/v1/batch" $ do
--   js <- jsonData
--   let rs = classifyBatchWith clf0 js
--   json rs







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



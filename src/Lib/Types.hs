{-# language DeriveGeneric #-}
module Lib.Types where

import GHC.Generics
import qualified Data.Aeson as J (FromJSON(..), ToJSON(..))
import Data.Csv (FromField(..), FromRecord(..), (.!))

import Lib.Math (V2)

-- | Batch of input data vectors to be classified
newtype Batch a = Batch { batch :: [V2 a]} deriving (Eq, Show, Generic)
instance J.FromJSON a => J.FromJSON (Batch a)

-- | Batch prediction
newtype Pred = Pred { prediction :: [Bool]} deriving (Eq, Show, Generic)
instance J.ToJSON Pred

-- | Model coefficients
data Coeffs a = Coeffs {
    bx :: a  -- ^ beta_x
  , by :: a  -- ^ beta_y
  , b0 :: a  -- ^ intercept
  } deriving (Eq, Show, Generic)
instance FromField a => FromRecord (Coeffs a)


-- | Data sample (labeled)
data Sample = Sample {
    sx :: Double    -- ^ x  
  , sy :: Double    -- ^ y
  , slabel :: Bool  -- ^ label
  } deriving (Eq, Show, Generic)

instance FromRecord Sample where
  parseRecord v =
    Sample <$> v .! 0 <*> v .! 1 <*> (toEnum <$> (v .! 2))

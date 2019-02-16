{-# language DeriveGeneric #-}
module Lib where

import GHC.Generics
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T (readFile)
import qualified Data.ByteString.Lazy as BS

import qualified Data.Vector as V

import qualified Data.Aeson as J (FromJSON(..), ToJSON(..), encode, decode)
import Data.Csv (decode, FromField(..), FromRecord(..), HasHeader(..), (.!))



-- | Inner product
(<.>) :: Num a => V2 a -> V2 a -> a
V2 ux uy <.> V2 vx vy = ux*vx + uy*vy

-- | Classify a point
classify :: (Ord a, Num a) => Coeffs a -> V2 a -> Bool
classify cs v = (betaV <.> v + beta0) > 0 where
  betaV = V2 (bx cs) (by cs)
  beta0 = b0 cs


-- | Decode a CSV document as coefficients (used for debugging)
decodeCoeffs :: BS.ByteString -> Either String (Coeffs Double)
decodeCoeffs bs = V.head <$> decode HasHeader bs

-- | Decode a CSV document as a vector of sample points (used for debugging)
decodeSamples :: BS.ByteString -> Either String (V.Vector Sample)
decodeSamples bs = decode HasHeader bs

-- | Decode a JSON blob as an array of points to be labeled
decodeBatch :: BS.ByteString -> Maybe (Batch Double)
decodeBatch bs = J.decode bs


-- | Usage:
-- 
-- λ> J.decode "[2,3.3]" :: Maybe (V2 Double)
-- Just (V2 2.0 3.3)
data V2 a = V2 a a deriving (Eq, Show, Generic)
instance FromField a => FromRecord (V2 a) where
  parseRecord v = V2 <$> v .! 0 <*> v .! 1
instance J.FromJSON a => J.FromJSON (V2 a)
instance J.ToJSON a => J.ToJSON (V2 a)

-- | Usage:
-- 
-- λ> J.decode "{\"batch\" : [[2,3.3], [1,2]]}" :: Maybe (Batch Double)
-- Just (Batch {batch = [V2 2.0 3.3,V2 1.0 2.0]})
newtype Batch a = Batch { batch :: [V2 a]} deriving (Eq, Show, Generic)
instance J.FromJSON a => J.FromJSON (Batch a)



-- | Model coefficients
data Coeffs a = Coeffs {
    bx :: a  -- ^ beta_x
  , by :: a  -- ^ beta_y
  , b0 :: a  -- ^ intercept
  } deriving (Eq, Show, Generic)
instance FromField a => FromRecord (Coeffs a)

-- | Provided model coefficients
coeffs0 :: Coeffs Double
coeffs0 = Coeffs 1.155907258055184 (-5.539862591450627) 0.8093445925050581


-- | Data sample (labeled)
data Sample = Sample {
    sx :: Double    -- ^ x  
  , sy :: Double    -- ^ y
  , slabel :: Bool  -- ^ label
  } deriving (Eq, Show, Generic)

instance FromRecord Sample where
  parseRecord v =
    Sample <$> v .! 0 <*> v .! 1 <*> (toEnum <$> (v .! 2))

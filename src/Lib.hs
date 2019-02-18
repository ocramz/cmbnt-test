{-|
Module      : Lib
Description : Library functions
Copyright   : (c) Marco Zocca, 2019
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX
-}
module Lib (
  -- * Classifying data
  classify, classifyBatchWith, coeffs0,
  fisherDiscriminant
  -- ** Math
  , (<.>)
  -- * Types
  , V2, Coeffs(..), Sample(..), Batch(..), Pred(..)
  -- * Decoding data
  -- ** from CSV 
  , decodeSamples, decodeCoeffs
  -- ** from JSON 
  , decodeJSONBatch
  ) where

import Lib.Types (Coeffs(..), Sample(..), Batch(..), Pred(..))
import Lib.Math (V2, mkV2, (<.>), meanV2, sampleCovariance, sumMat2, (<\>), (^-^))

import Data.Either (partitionEithers)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T (readFile)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.Aeson as J (decode)
import Data.Csv (decode, HasHeader(..))


-- | Classify a point
classify :: (Ord a, Num a) => Coeffs a -> V2 a -> Bool
classify cs v = (betaV <.> v + beta0) > 0 where
  betaV = mkV2 (bx cs) (by cs)
  beta0 = b0 cs

-- | Classify a batch of points with some binary point classifier
classifyBatchWith :: (V2 a -> Bool) -> Batch a -> Pred
classifyBatchWith classf bs = Pred $ map classf (batch bs)


-- | Decode a JSON blob as an array of points to be labeled
decodeJSONBatch :: BS.ByteString -> Maybe (Batch Double)
decodeJSONBatch = J.decode 


-- | Provided model coefficients
coeffs0 :: Coeffs Double
coeffs0 = Coeffs 1.155907258055184 (-5.539862591450627) 0.8093445925050581


-- | Decode a CSV document as coefficients (used for debugging)
decodeCoeffs :: BS.ByteString -> Either String (Coeffs Double)
decodeCoeffs bs = V.head <$> decode HasHeader bs

-- | Decode a CSV document as a vector of sample points (used for debugging)
decodeSamples :: BS.ByteString -> Either String (V.Vector Sample)
decodeSamples = decode HasHeader 


-- | Fisher linear discriminant
--
-- Returns the direction of maximum separation between the two classes (i.e. the separating plane is orthogonal to this vector)
fisherDiscriminant :: Foldable t => t Sample -> V2 Double
fisherDiscriminant xs = (sig0 `sumMat2` sig1) <\> (mu1 ^-^ mu0) where
  (xs0, xs1) = partitionSamples xs
  mu0 = meanV2 xs0
  mu1 = meanV2 xs1
  sig0 = sampleCovariance xs0
  sig1 = sampleCovariance xs1

-- | Partition a labeled sample 
partitionSamples :: Foldable t => t Sample -> ([V2 Double], [V2 Double])
partitionSamples xs = partitionEithers $ foldr insf [] xs where
  insf (Sample ssx ssy lab) acc | lab       = Left (mkV2 ssx ssy) : acc
                                | otherwise = Right (mkV2 ssx ssy) : acc

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
  -- ** Default affine classifier
  classify, classifyBatchWith
  -- *** Preset coefficients
  , coeffs0, vcoeffs0  
  -- ** Fisher linear discriminant (FDA)
  , fisherDiscriminant
  , fda
  -- ** Quadratic discriminant analysis (QDA)
  , qda 
  -- ** Math
  , (<.>)
  -- * Types
  , V2, Mat2, Coeffs(..), Sample(..), Batch(..), Pred(..), ClassifierMethod(..), ClassifierConfig(..), classifierConfigDefault
  -- * Decoding data
  -- ** from CSV 
  , decodeSamples, decodeCoeffs
  -- ** from JSON 
  , decodeJSONBatch
  ) where

import Lib.Types (Coeffs(..), Sample(..), Batch(..), Pred(..), ClassifierMethod(..), ClassifierConfig(..), classifierConfigDefault)
import Lib.Math (V2, Mat2, mkV2, (<.>), meanV2, sampleCovariance, sumMat2, (<\>), (^-^))

import Data.Ord (comparing)
import Data.List (minimumBy)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T (readFile)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.Aeson as J (decode)
import Data.Csv (decode, HasHeader(..))


-- | Classify a point with the default affine classifier
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

vcoeffs0 :: V2 Double
vcoeffs0 = mkV2 bx' by' where
  (Coeffs bx' by' _) = coeffs0


-- | Decode a CSV document as coefficients (used for debugging)
decodeCoeffs :: BS.ByteString -> Either String (Coeffs Double)
decodeCoeffs bs = V.head <$> decode HasHeader bs

-- | Decode a CSV document as a vector of sample points (used for debugging)
decodeSamples :: BS.ByteString -> Either String (V.Vector Sample)
decodeSamples = decode HasHeader 


-- * Classification utilities

-- | Classify a point according to QDA (Quadratic discriminant analysis) (with uniform class priors)
--
-- This computes the Mahalanobis distance of a test point to each of the training data clusters and returns the class index (a Boolean, since we restrict to binary classification here) of the closest cluster.
--
-- see Friedman 1989
qda :: Foldable t => t Sample -> V2 Double -> Bool
qda xs x =
  fst $ 
  minimumBy (comparing snd) $
  zip [True, False] $
  map (`mahalanobisDist` x) [xs0, xs1]
  where
    (xs0, xs1) = partitionSamples xs  

-- | Classify a point according to FDA (Fisher (linear) discriminant analysis)
--
-- Returns True if w^T x >= 0 where w is the Fisher vector
fda :: Foldable t => t Sample -> V2 Double -> Bool
fda xs x = w <.> x >= 0 where
  w = fisherDiscriminant xs

-- | Fisher linear discriminant analysis
--
-- Returns the direction of maximum separation between the two classes (i.e. the discriminating plane is orthogonal to the result vector)
fisherDiscriminant :: Foldable t => t Sample -> V2 Double
fisherDiscriminant xs = (sig0 `sumMat2` sig1) <\> (mu0 ^-^ mu1) where
  (xs0, xs1) = partitionSamples xs
  (mu0, sig0) = sampleStats xs0
  (mu1, sig1) = sampleStats xs1

-- | Mahalanobis distance (squared) between x and the mean vector of the dataset, as measured by the dataset covariance
mahalanobisDist :: (Floating a, Foldable t) => t (V2 a) -> V2 a -> a
mahalanobisDist xs x = xc <.> (sig <\> xc) where
  (mu, sig) = sampleStats xs
  xc = x ^-^ mu

-- | Compute mean and covariance matrix of a dataset
sampleStats :: (Fractional a, Foldable t) => t (V2 a) -> (V2 a, Mat2 a)
sampleStats xs = (mu, sig) where
  mu = meanV2 xs
  sig = sampleCovariance xs
    

-- | Partition a labeled sample
--
-- If the label is True, a point goes in the left partition
partitionSamples :: Foldable t => t Sample -> ([V2 Double], [V2 Double])
partitionSamples xs = foldr insf ([], []) xs where
  insf (Sample ssx ssy lab) (l, r) | lab       = (mkV2 ssx ssy : l, r)
                                   | otherwise = (l, mkV2 ssx ssy : r)

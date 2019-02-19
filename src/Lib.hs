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
  -- * Training a classifier
  , train
  -- ** Fisher linear discriminant (FDA)
  , fda
  -- ** Quadratic discriminant analysis (QDA)
  , qda 
  -- * Types
  , V2, Mat2, Coeffs(..), Sample(..), Batch(..), Pred(..), ClassifierMethod(..), ClassifierConfig(..), classifierConfigDefault
  -- * Loading data
  , samples
  -- * Decoding data
  -- ** from CSV 
  , decodeSamples, decodeCoeffs
  -- ** from JSON 
  , decodeJSONBatch
  ) where

import Lib.Types (Coeffs(..), Sample(..), Batch(..), Pred(..), ClassifierMethod(..), ClassifierConfig(..), classifierConfigDefault)
import Lib.Math (V2, Mat2, mkV2, (<.>), meanV2, sampleCovariance, sumMat2, detMat2, (<\>), (^-^))

import Data.Ord (comparing)
import Data.List (minimumBy)
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


-- | Default model coefficients for the default linear classifier
coeffs0 :: Coeffs Double
coeffs0 = Coeffs 1.155907258055184 (-5.539862591450627) 0.8093445925050581

-- | Default model coefficients for the default linear classifier (as a V2, disregarding the offset)
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

-- | Train a classifier
train ::
     ClassifierConfig  -- ^ Configuration (training dataset and classifier method)
  -> (V2 Double -> Bool) -- ^ Point classifier
train (ClassifierConfig sxs cty) | cty == FDA = fda sxs
                                 | otherwise  = qda sxs 

-- | Classify a point according to FDA (Fisher (linear) discriminant analysis)
--
-- Returns True if w^T x >= 0 where w is the Fisher (= maximum separation) vector
--
-- NB : the query is performed with a "centered" query vector (i.e. the dataset mean is subtracted from the query point).
fda :: (Functor t, Foldable t) => t Sample -> V2 Double -> Bool
fda sxs x = w <.> xc >= 0 where
  w = fisherV sxs
  xc = x ^-^ meanV2 (sampleGetV2 <$> sxs)


-- | Direction of maximum separation between the two classes (i.e. the discriminating plane is orthogonal to the result vector) 
fisherV :: Foldable t => t Sample -> V2 Double
fisherV sxs = (sig0 `sumMat2` sig1) <\> (mu0 ^-^ mu1) where
  (xs0, xs1) = partitionSamples sxs
  (mu0, sig0) = sampleStats xs0
  (mu1, sig1) = sampleStats xs1

-- | Classify a point according to QDA (Quadratic discriminant analysis) (with uniform class priors)
--
-- This computes the Mahalanobis distance of a test point to each of the training data clusters and returns the class index (a Boolean, since we restrict to binary classification here) of the closest cluster.
--
-- see J. H. Friedman, Regularized discriminant analysis, 1989
qda :: Foldable t => t Sample -> V2 Double -> Bool
qda xs x =
  fst $ 
  minimumBy (comparing snd) $
  zip [True, False] $
  map (`discriminantQDA` x) [xs0, xs1]
  where
    (xs0, xs1) = partitionSamples xs  


-- | Discriminant function used in QDA
--
-- Internally computes the Mahalanobis distance (squared) between x and the mean vector of the dataset, as measured by the dataset covariance.
discriminantQDA :: (Floating a, Foldable t) => t (V2 a) -> V2 a -> a
discriminantQDA xs x = mahd + log (detMat2 sig)
  where
    mahd = xc <.> (sig <\> xc)
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
partitionSamples = foldr insf ([], []) where
  insf (Sample ssx ssy lab) (l, r) | lab       = (mkV2 ssx ssy : l, r)
                                   | otherwise = (l, mkV2 ssx ssy : r)

sampleGetV2 :: Sample -> V2 Double
sampleGetV2 (Sample vx vy _) = mkV2 vx vy


-- | Load and parse the training samples from disk.
--
-- If the parse fails, the list will be empty
samples ::
     String   -- ^ File path of samples.csv
  -> IO [Sample]
samples dpath = do
  ss <- BS.readFile dpath 
  pure $ V.toList $ either (const V.empty) id $ decodeSamples ss  

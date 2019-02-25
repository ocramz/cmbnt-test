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
  classifyBatchWith
  -- * Training a classifier
  , train
  -- * Evaluating a classifier
  , distanceMin
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



-- | Classify a batch of points with some binary point classifier
classifyBatchWith :: (V2 a -> Bool) -> Batch a -> Pred
classifyBatchWith classf bs = Pred $ map classf (batch bs)


-- | Decode a JSON blob as an array of points to be labeled
decodeJSONBatch :: BS.ByteString -> Maybe (Batch Double)
decodeJSONBatch = J.decode 


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

-- | Compute the minimum distance function 
distanceMin ::
     ClassifierConfig  -- ^ Configuration (training dataset and classifier method)
  -> (V2 Double -> Double)  -- ^ Distance function
distanceMin (ClassifierConfig sxs cty) | cty == FDA = distanceMinFDA sxs
                                       | otherwise = distanceMinQDA sxs

-- | Classify a point according to FDA (Fisher (linear) discriminant analysis)
--
-- Returns True if w^T x >= 0 where w is the Fisher (= maximum separation) vector
--
-- NB : the query is performed with a "centered" query vector (i.e. the dataset mean is subtracted from the query point).
fda :: (Functor t, Foldable t) => t Sample -> V2 Double -> Bool
fda sxs x = w <.> xc >= 0 where
  w = fisherV sxs
  xc = x ^-^ meanV2 (sampleGetV2 <$> sxs)

distanceMinFDA :: Foldable t =>
     t Sample
  -> (V2 Double -> Double)  -- ^ Distance function
distanceMinFDA sxs x = min (w <.> xc0) (w <.> xc1)
  where
    w = fisherV sxs
    xc0 = x ^-^ mu0
    xc1 = x ^-^ mu1
    (xs0, xs1) = partitionSamples sxs
    (mu0, _) = sampleStats xs0
    (mu1, _) = sampleStats xs1    
    


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

-- | Minimum class distance
distanceMinQDA :: Foldable t =>
     t Sample  -- ^ Training set
  -> (V2 Double -> Double)  -- ^ Distance function
distanceMinQDA xs x = min (discriminantQDA xs0 x)( discriminantQDA xs1 x)
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

module Lib (
  -- * Classifying data
  classify, classifyBatchWith, coeffs0
  -- ** Math
  , (<.>)
  -- * Types
  , V2(..), Coeffs(..), Sample(..), Batch(..), Pred(..)
  -- * Decoding data
  -- ** from CSV 
  , decodeSamples, decodeCoeffs
  -- ** from JSON 
  , decodeJSONBatch
  ) where

import Lib.Types (V2(..), Coeffs(..), Sample(..), Batch(..), Pred(..))

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T (readFile)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import qualified Data.Aeson as J (decode)
import Data.Csv (decode, HasHeader(..))



-- | Inner product of two vectors
(<.>) :: Num a => V2 a -> V2 a -> a
V2 ux uy <.> V2 vx vy = ux*vx + uy*vy

-- | Classify a point
classify :: (Ord a, Num a) => Coeffs a -> V2 a -> Bool
classify cs v = (betaV <.> v + beta0) > 0 where
  betaV = V2 (bx cs) (by cs)
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

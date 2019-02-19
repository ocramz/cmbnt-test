{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-|
Module      : Lib.Math
Description : Math and stats library functions
Copyright   : (c) Marco Zocca, 2019
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX
-}
module Lib.Math (
  sampleCovariance, 
  -- * Vectors in 2D
  V2, mkV2, (<.>), (^+^), (^-^), meanV2, norm2, normalize2, (<^>),  
  -- * (2 * 2) matrices
  Mat2(..), trace, frobenius, sumMat2, detMat2, scaleMat2, transpose, (##),
  (=~=), 
  chol2, fwdSubst, bwdSubst, 
  -- * Matrix-vector operations
  (#>), 
  -- ** Solving linear systems
  (<\>), 
  -- * Utilities
  v2x, v2y
  )where

import GHC.Generics
import qualified Data.Aeson as J (FromJSON(..), ToJSON(..))
import Data.Csv (FromField(..), FromRecord(..), ToField(..), ToRecord(..), (.!))


-- | Vector in 2d
data V2 a = V2 a a deriving (Eq, Show, Generic)
instance ToField a => ToRecord (V2 a)
instance FromField a => FromRecord (V2 a) where
  parseRecord v = V2 <$> v .! 0 <*> v .! 1
instance J.FromJSON a => J.FromJSON (V2 a)
instance J.ToJSON a => J.ToJSON (V2 a)

-- | Inner product of two vectors
(<.>) :: Num a => V2 a -> V2 a -> a
V2 ux uy <.> V2 vx vy = ux*vx + uy*vy

-- | L2 norm of a vector
norm2 :: Floating a => V2 a -> a
norm2 v = sqrt $ v <.> v

-- | Normalize a vector w.r.t its L2 norm
normalize2 :: Floating a => V2 a -> V2 a
normalize2 v = scaleV2 (recip z) v where z = norm2 v

-- | Construct a 2D vector from its components
mkV2 :: a -> a -> V2 a
mkV2 = V2

-- | Access the components of a V2
v2x, v2y :: V2 a -> a
v2x (V2 x _) = x
v2y (V2 _ y) = y

-- | Outer product of two vectors
(<^>) :: Num a => V2 a -> V2 a -> Mat2 a
(V2 u1 u2) <^> (V2 v1 v2) = Mat2 a11 a12 a21 a22 where
  a11 = u1 * v1
  a12 = u1 * v2
  a21 = u2 * v1
  a22 = u2 * v2

-- | Vector sum
(^+^):: Num a => V2 a -> V2 a -> V2 a
(^+^) (V2 u1 v1) (V2 u2 v2) = V2 (u1 + u2) (v1 + v2)

-- | Vector difference
(^-^) :: Num a => V2 a -> V2 a -> V2 a
u ^-^ v = u ^+^ scaleV2 (-1) v

-- | Scale a vector
scaleV2 :: Num a => a -> V2 a -> V2 a
scaleV2 s (V2 u v) = V2 (s * u) (s * v)

-- | Mean vector
meanV2 :: (Fractional a, Foldable t) => t (V2 a) -> V2 a
meanV2 vs = scaleV2 (recip n) $ foldr (^+^) (V2 0 0) vs where
  n = fromIntegral $ length vs

-- | Dense (2 * 2) matrix
data Mat2 a = Mat2 a a a a deriving (Eq, Show)

-- | Matrix trace
trace :: Num a => Mat2 a -> a
trace (Mat2 a11 _ _ a22) = a11 + a22

-- | Frobenius norm of a matrix
frobenius :: Num a => Mat2 a -> a
frobenius m = trace (transpose m ## m)

-- | Matrix determinant
detMat2 :: Num a => Mat2 a -> a
detMat2 (Mat2 a11 a12 a21 a22) = a11 * a22 - a21 * a12

-- | Elementwise sum of two matrices
sumMat2 :: Num a => Mat2 a -> Mat2 a -> Mat2 a
sumMat2 (Mat2 a11 a12 a21 a22) (Mat2 b11 b12 b21 b22) =
  Mat2 (a11 + b11) (a12 + b12) (a21 + b21) (a22 + b22)

scaleMat2 :: Num a => a -> Mat2 a -> Mat2 a
scaleMat2 s (Mat2 a11 a12 a21 a22) = Mat2 (s * a11) (s * a12) (s * a21) (s * a22)

-- | Product of two matrices (= operator composition)
prodMat2 :: Num a => Mat2 a -> Mat2 a -> Mat2 a
prodMat2 (Mat2 a11 a12 a21 a22) (Mat2 b11 b12 b21 b22) = Mat2 a b c d where
  a = a11 * b11 + a12 * b21
  b = a11 * b12 + a12 * b22
  c = a21 * b11 + a22 * b21
  d = a21 * b12 + a22 * b22

-- | Product of two matrices (= operator composition)
(##) :: Num a => Mat2 a -> Mat2 a -> Mat2 a
(##) = prodMat2

-- | Sample covariance matrix
--
-- NB : Full rank iff the number of data points is larger than the dimensionality (in this case, iff N > 2)
sampleCovariance :: (Fractional a, Foldable t) => t (V2 a) -> Mat2 a
sampleCovariance xs = scaleMat2 (recip (n - 1)) $ foldr accf mat0 xs where
  n = fromIntegral $ length xs 
  accf v macc = vc <^> vc `sumMat2` macc where
    vc = v ^-^ vmean
  vmean = meanV2 xs
  mat0 = Mat2 0 0 0 0
  
-- | Compute the Cholesky factor of a (2 * 2) matrix
chol2 :: Floating a => Mat2 a -> Mat2 a
chol2 (Mat2 a11 _ a21 a22) = Mat2 l11 0 l21 l22 where
  l11 = sqrt a11
  l21 = a21 / l11
  l22 = sqrt (a22 - l21 ** 2)

-- | Solve a (2 * 2) /positive definite/ linear system via the Cholesky factorization
--
-- NB : does not use pivoting so the diagonal elements of the matrix must be positive.
linSolve2 :: Floating a => Mat2 a -> V2 a -> V2 a
linSolve2 m v = bwdSubst (transpose l) p where
  l = chol2 m
  p = fwdSubst l v

-- | Solve a (2 * 2) /positive definite/ linear system via the Cholesky factorization
--
-- NB : does not use pivoting so the diagonal elements of the matrix must be positive.
(<\>) :: Floating a => Mat2 a -> V2 a -> V2 a
(<\>) = linSolve2

-- | Solve a lower-triangular (2 * 2) linear system by forward substitution
fwdSubst :: Fractional a => Mat2 a -> V2 a -> V2 a
fwdSubst (Mat2 l11 _ l21 l22) (V2 b1 b2) = V2 x1 x2 where
  x1 = b1 / l11
  x2 = (b2 - l21 * x1) / l22

-- | Solve an upper-triangular (2 * 2) linear system by backward substitution
bwdSubst :: Fractional a => Mat2 a -> V2 a -> V2 a
bwdSubst (Mat2 l11 l12 _ l22) (V2 b1 b2) = V2 x1 x2 where
  x2 = b2 / l22
  x1 = (b1 - l12 * x2) / l11

-- | Matrix transpose
transpose :: Mat2 a -> Mat2 a
transpose (Mat2 a11 a12 a21 a22) = Mat2 a11 a21 a12 a22

-- | Matrix-vector action
(#>) :: Num a => Mat2 a -> V2 a -> V2 a
(Mat2 a11 a12 a21 a22) #> (V2 u v) = V2 (a11 * u + a12 * v) (a21 * u + a22 * v)



-- | Approximate comparison for floating-point math
--
-- This shouldn't be abused because errors accumulate so the precision decreases throughout a computation
class (Eq a) => Eps a where
  (=~=) :: a -> a -> Bool
  (=~=) = (==)

-- | Approximate comparison of two matrices
--
-- The Frobenius norm of the difference should be less than the given floating point precision (e.g. 1e-12 for Double)
instance Eps (Mat2 Double) where
  mm1 =~= mm2 = frobenius (mm1 `sumMat2` scaleMat2 (- 1) mm2) <= 1e-12

instance Eps (V2 Double) where
  v1 =~= v2 = norm2 (v1 ^-^ v2) <= 1e-12

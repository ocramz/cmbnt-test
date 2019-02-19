module LibSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Lib
import Lib.Math

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Lib/Types" $ do 
    it "decodes data/model.csv" $ do
      d <- BS.readFile "data/model.csv"
      let cs = decodeCoeffs d
      cs `shouldBe` Right Coeffs {bx = 1.155907258055184, by = -5.539862591450627, b0 = 0.8093445925050581}
    it "decodes data/samples.csv" $ do
      d <- BS.readFile "data/samples.csv"
      let sxs = decodeSamples d
      V.length <$> sxs `shouldBe` Right 10
  describe "Lib/Math" $ do
    it "computes the inner product of two vectors" $ 
      e1 <.> e2 `shouldBe` 0    
    it "computes the mean of two sample vectors" $ do
      let v1 = mkV2 0 0 :: V2 Double
          v2 = mkV2 1 1
      meanV2 [v1, v2] `shouldBe` mkV2 0.5 0.5
    it "computes the outer product of two vectors" $ do
      let v = mkV2 1 2 :: V2 Double
      v <^> v `shouldBe` Mat2 1 2 2 4
    it "computes the Cholesky factor of a PD matrix" $ do
      let l = chol2 mcov
      (l ## transpose l) =~= mcov `shouldBe` True
    it "solves a PD linear system via the Cholesky factorization" $ do
      let b = mkV2 1.8 (- 2.3)
          x = mcov <\> b
      (mcov #> x) =~= b `shouldBe` True
  describe "Lib" $ do
    it "correctly classifies provided data with the pre-trained coefficients" $ do
      d <- BS.readFile "data/model.csv"
      let cs = either (\e -> error $ unwords ["data/model.csv not found or malformed", e]) id $ decodeCoeffs d
      classify cs vtest `shouldBe` True    
    it "classifies test points with the Fisher linear discriminant" $ do
      sxs <- samples
      fda sxs vTestTrue `shouldBe` True
      fda sxs vTestTrue1 `shouldBe` True      
      fda sxs vTestFalse `shouldBe` False
    it "classifies test points with QDA" $ do
      sxs <- samples
      qda sxs vTestTrue `shouldBe` True
      qda sxs vTestTrue1 `shouldBe` True      
      qda sxs vTestFalse `shouldBe` False

-- | Load the samples from disk and parse them.
--
-- Crash if the samples file is not found or cannot be parsed
samples :: IO (V.Vector Sample)
samples = do
  d <- BS.readFile "data/samples.csv"
  pure $ either (\e -> error $ unwords ["data/model.csv not found or malformed", e]) id $ decodeSamples d






 
-- | Test data

mcov :: Mat2 Double
mcov = sampleCovariance [v1, v2, v3, v4, v5] where
  v1 = mkV2 1 2
  v2 = mkV2 2 3
  v3 = mkV2 3 4
  v4 = mkV2 (-1) 4
  v5 = mkV2 (-2) 5

vTestTrue, vTestTrue1, vTestFalse :: V2 Double
-- | these points should classify as True (or, class 0)
vTestTrue = mkV2 1.7 (- 0.3)
vTestTrue1 = mkV2 1.8 0.2
-- | this point should classify as False (or, class 1)
vTestFalse = mkV2 0.1 0.75

vtest :: V2 Double
vtest = mkV2 1.97 0.17

e1, e2 :: V2 Double
e1 = mkV2 1 0
e2 = mkV2 0 1

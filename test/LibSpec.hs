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
  describe "Lib" $ do
    it "computes the inner product of two vectors" $ 
      e1 <.> e2 `shouldBe` 0
    it "correctly classifies provided data with the pre-trained coefficients" $ do
      d <- BS.readFile "data/model.csv"
      let cs = either (\e -> error $ unwords ["data/model.csv not found or malformed", e]) id $ decodeCoeffs d
      classify cs vtest `shouldBe` True
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


vTestTrue, vTestTrue1, vTestFalse :: V2 Double
-- | these points should classify as True (or, class 0)
vTestTrue = mkV2 1.7 (- 0.3)
vTestTrue1 = mkV2 1.8 0.2
-- | this point should classify as False (or, class 1)
vTestFalse = mkV2 0.1 0.6 

vtest :: V2 Double
vtest = mkV2 1.97 0.17

e1, e2 :: V2 Double
e1 = mkV2 1 0
e2 = mkV2 0 1

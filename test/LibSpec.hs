module LibSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V

import Lib 

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Lib" $ do
    it "computes the inner product of two vectors" $ 
      e1 <.> e2 `shouldBe` 0
    it "correctly classifies provided data" $ do
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

vtest :: V2 Double
vtest = V2 1.97 0.17

e1, e2 :: V2 Double
e1 = V2 1 0
e2 = V2 0 1

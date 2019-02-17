module LibSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V


-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T (readFile)

import Lib 

main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "Lib" $ do
    it "decodes data/model.csv" $ do
      d <- BS.readFile "data/model.csv"
      let cs = decodeCoeffs d
      cs `shouldBe` Right Coeffs {bx = 1.155907258055184, by = -5.539862591450627, b0 = 0.8093445925050581}
    it "decodes data/samples.csv" $ do
      d <- BS.readFile "data/samples.csv"
      let sxs = decodeSamples d
      V.length <$> sxs `shouldBe` Right 10
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x




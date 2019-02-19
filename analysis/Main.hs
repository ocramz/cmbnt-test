{-# language DeriveGeneric #-}
module Main where

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import Data.Char

import Lib (train, V2, samples, ClassifierConfig(..), ClassifierMethod(..))
import Lib.Math

import qualified Data.Csv as C (ToRecord(..), ToField(..), encode)

-- | Train and evaluate a classifier over a grid of points, to be plotted as a heatmap or contour plot
main :: IO ()
main = do
  sxs <- samples "data/samples.csv"
  let cc = ClassifierConfig sxs QDA
      classf x = if train cc x then 1 else 0
      ds = dats 0.1 0.1 (-3) 1.5 (-2) 0.5
      dsf = (\d -> LSamp (v2x d) (v2y d) (classf d) ) `map` ds
      csv = C.encode dsf
      fname =
        mconcat
          ["analysis/samples_grid_", map toLower (show $ clcClassifier cc), ".csv"]
  BS.writeFile fname csv
  putStrLn $ unwords ["Output file written:", fname]

data LSamp a = LSamp a a Int deriving (Eq, Show, Generic)
instance C.ToField a => C.ToRecord (LSamp a)

dats :: (Num a, Enum a) => a -> a -> a -> a -> a -> a -> [V2 a]
dats dx dy xmin xmax ymin ymax = [mkV2 x y | x <- xs, y <- ys] where
  xs = [xmin, xmin + dx .. xmax - dx]
  ys = [ymin, ymin + dy .. ymax - dy]


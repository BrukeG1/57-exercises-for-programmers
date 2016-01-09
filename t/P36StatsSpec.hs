module P36StatsSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P36Stats hiding (main)

import Data.Vector (fromList)
import qualified Statistics.Sample as S

stdDevSample = S.stdDev . fromList
stdDevPop    = sqrt . S.variance . fromList
statMean     = S.mean . fromList

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "avg" $ do
    it "correctly returns the mean average of a list" $ do
      avg [1,2,3] `shouldBe` 2
    prop "is within 0.00000001 of mean from Statistics.Sample library (some rounding errors exist)" $
      \xs -> avg xs - statMean xs <0.00000001
  describe "sampSD" $ do
    prop "is within 0.0000000001 of stdDev from Statistics.Sample library (some rounding errors exist)" $
      \xs -> sampSD xs - stdDevSample xs <0.0000000001
  describe "popSD" $ do
    prop "is within 0.0000000001 of sqrt of variance from Statistics.Sample library (some rounding errors exist)" $
      \xs -> popSD xs - stdDevPop xs <0.0000000001

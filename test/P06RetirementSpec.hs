module P06RetirementSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P06Retirement hiding (main)

testYear :: Int
testYear = 2016

main :: IO ()
main = hspec spec

spec = do
  describe "retireWhen" $ do
    it "returns past when retirement year in past" $ do
      retireWhen 67 65 testYear `shouldBe` Past (testYear - 2)
    it "returns present when retirement year is now" $ do
      retireWhen 65 65 testYear `shouldBe` Now testYear
    it "returns future when retirement year is in future" $ do
      retireWhen 65 67 testYear `shouldBe` Future (testYear + 2)
    prop "always gives a correct past present or future given 2 ints for years" $
          \x y ->
            let yr = testYear + (y - x) in
            case compare x y of
              GT -> retireWhen x y testYear `shouldBe` Past yr
              EQ -> retireWhen x y testYear `shouldBe` Now yr
              LT -> retireWhen x y testYear `shouldBe` Future yr


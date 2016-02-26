module P22CompNumsSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P22CompNums hiding (main)
import Data.List(sort)

emptyList :: [Int]
emptyList = []

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "max'" $ do
      it "gives Nothing as the max of an empty list" $ do
        max' Nothing emptyList `shouldBe` Nothing
      it "gives 4 as the max of [4,1]" $ do
        max' Nothing [4,1] `shouldBe` Just 4
      it "gives 4 as the max of [1,4]" $ do
        max' Nothing [1,4] `shouldBe` Just 4
    describe "maxInt" $ do
      it "gives Nothing as the max of an empty list" $ do
        maxInt emptyList `shouldBe` Nothing
      prop "has the property that maxInt of a nonempty list is the same as Just the last element of that list sorted" $
        \xs -> not (null xs) ==> maxInt xs `shouldBe` Just (last $ sort xs)

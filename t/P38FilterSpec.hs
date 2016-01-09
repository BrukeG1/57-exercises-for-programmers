module P38FilterSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P38Filter hiding (main)
 
 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "filt" $ do
    it "filters even numbers when passed evn as an predicate and a list of numbers" $ do
      filt evn [1,2,3,4] `shouldBe` [2,4]
    it "returns an empty list when passed evn as an predicate and a list of numbers with no evens" $ do
      filt evn [1,3,5,7,9] `shouldBe` []
    prop "has the same behaviour as filter when called with evn as a predicate and some list xs" $
      \xs -> filter evn xs == filt evn xs
  describe "evn" $ do
    it "identifies 2 as even" $ do
      evn 2 `shouldBe` True
    it "identifies 3 as odd" $ do
      evn 3 `shouldBe` False
    prop "has the same behaviour as even from the prelude" $
          \x -> evn x == even x
  describe "od" $ do
    it "identifies 3 as odd" $ do
      od 3 `shouldBe` True
    it "identifies 2 as not odd" $ do
      od 2 `shouldBe` False
    prop "has the same behaviour as odd from the prelude" $
          \x -> od x == odd x

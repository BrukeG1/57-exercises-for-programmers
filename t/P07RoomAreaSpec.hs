module P07RoomAreaSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P07RoomArea hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "f2ed" $ do
    it "calculates the area of a square correctly" $ do
      f2ed 3 3 `shouldBe` 9
    it "calculates the area of a rectangle correctly" $ do
      f2ed 3 2 `shouldBe` 6
  describe "f2tom2" $ do
    it "should calculate pretty much the same figure as google does (ours is more precise)" $ do
      (f2tom2 9) - 0.836127 `shouldSatisfy` (<0.000001) 

module P09PaintSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P09Paint hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "tinsOfPaint" $ do
    it "returns 1 when area == coverage" $ do
      tinsOfPaint coverage `shouldBe` 1
    it "returns 2 when area > coverage && area < 2* coverage" $ do
      tinsOfPaint (coverage*2-1) `shouldBe` 2
    it "returns 2 when area == 2 * coverage" $ do
      tinsOfPaint (coverage*2) `shouldBe` 2
  describe "radius" $ do
    it "calculates radius for diameter 4 ok" $ do
      radius 4 `shouldBe` 2.0
    prop "has the property that radius 2x == x for all x" $
      \x -> radius (x*2) == fromIntegral x
  describe "circArea" $ do
    prop "has the property of working out the same as pi r squared, rounded" $
      \x -> circArea x `shouldBe` round (pi * (radius x**2)::Float)
    it "gives the same area as google for a circle of radius 18, diam 36 (~1017.88 =~ 1018)" $ do
      circArea 36 `shouldBe` 1018
  describe "rectArea" $ do
    it "gives the same answer as google for a rectangle of length 5, width 3" $ do
      rectArea 5 3 `shouldBe` 15
  describe "elArea" $ do
    it "gives the same result as http://www.had2know.com/househome/l-shaped-room-area-perimeter-calculator.html for a room of long part 10,8, l part 4,4" $ do
      elArea 10 8 4 4 `shouldBe` 64

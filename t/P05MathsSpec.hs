module P05MathsSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P05Maths hiding (main)

main :: IO ()
main = hspec spec

x, y :: Float
x = 120
y = 9

spec = do
  describe "plus" $ do
    it "is the same as adding 2 floats and calling show" $ do
      x `plus` y `shouldBe` "129.0"
    it "is commutative" $ do
      x `plus` y `shouldBe` y `plus` x
  describe "minus" $ do
    it "is the same as subtracting 1 floats from another and calling show on the result" $ do
      x `minus` y `shouldBe` "111.0"
  describe "divBy" $ do
    it "is the same as dividing 1 float by another and calling show on the result" $ do
      x `divBy` y `shouldBe` "13.333333" -- may be architecture dependent
    it "is the same as dividing 1 float by another and calling show on the result" $ do
      y `divBy` x `shouldBe` "7.5e-2"
  describe "times" $ do
    it "is the same as multiplying 1 float by another and calling show on the result" $ do
      x `times` y `shouldBe` "1080.0" -- may be architecture dependent

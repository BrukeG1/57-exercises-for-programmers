module P05MathsSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P05Maths hiding (main)

main :: IO ()
main = hspec spec

x, y :: Float
x = 120
y = 9

spec :: Spec
spec = do
  describe "plus" $ do
    it "is the same as adding 2 floats and calling show" $
      x `plus` y `shouldBe` 129.0
    prop "has the property of commutativity" $
      \a b -> a `plus` b `shouldBe` b `plus` a
    prop "has the property of associativity [up to 3 decimal places] (a+b)+c==a+(b+c) " $
      \a b c -> ((a `plus` b) `plus` c) - (a `plus` (b `plus` c)) < 0.001
  describe "minus" $
    it "is the same as subtracting 1 floats from another and calling show on the result" $
      x `minus` y `shouldBe` 111.0
  describe "divBy" $ do
    it "is the same as dividing 1 float by another and calling show on the result" $
      x `divBy` y `shouldBe` 13.333333
    it "is the same as dividing 1 float by another and calling show on the result" $
      y `divBy` x `shouldBe` 7.5e-2
  describe "times" $ do
    it "is the same as multiplying 1 float by another and calling show on the result" $
      x `times` y `shouldBe` 1080.0
    prop "has the property of commutativity" $
      \a b -> a `times` b `shouldBe` b `times` a

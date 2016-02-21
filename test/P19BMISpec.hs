module P19BMISpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P19BMI hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "toInches" $ do
      prop "has the property that toInches (x inches) == x inches" $
        \x   -> toInches (Inches x) == Inches x
      prop "has the property that toInches (x ft y inches) == 12x + y inches" $
        \x y -> toInches (FeetInches x y) == Inches (12 * x + y)
      prop "has the property that toInches (x cm) == x / 2.54 inches" $
        \x   -> toInches (Centimeters x) == Inches (x/2.54)
    describe "toPounds" $ do
      prop "has the property that toPounds (x lb) == x lb" $
        \x   -> toPounds (Pounds x) == Pounds x
      prop "has the property that toPounds (x kg) == x * 2.205 lb" $
        \x   -> toPounds (Kilograms x) == Pounds (x * 2.205)
    describe "cmpBMI" $ do
      it "gives Low for low BMI" $ do
        cmpBMI 18.49 `shouldBe` Low
      it "gives Healthy for normal BMI" $ do
        cmpBMI 21.49 `shouldBe` Healthy
      it "gives High for high BMI" $ do
        cmpBMI 25.001 `shouldBe` High
    describe "bmi" $ do
      prop "has the property of calculating BMI for height/weight the same as using the formula directly" $
        \h w -> h>0 ==> bmi (Inches h) (Pounds w) == (w/(h**2)) * 703

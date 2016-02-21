module P19BMISpec (main,spec) where

import Test.Hspec
import P19BMI hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "toInches" $ do
      it "is pending" $
        pending
    describe "toPounds" $ do
      it "is pending" $
        pending
    describe "cmpBMI" $ do
      it "is pending" $
        pending
    describe "bmi" $ do
      it "is pending" $
        pending

module P21NumToNameSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import Control.Exception(evaluate) -- for testing errors thrown by pure code
import P21NumToName hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "getMonth" $ do
      it "gives January on input 1" $
        getMonth 1 `shouldBe` "January"
      it "gives February on input 2" $
        getMonth 2 `shouldBe` "February"
      it "gives March on input 3" $
        getMonth 3 `shouldBe` "March"
      it "gives April on input 4" $
        getMonth 4 `shouldBe` "April"
      it "gives May on input 5" $
        getMonth 5 `shouldBe` "May"
      it "gives June on input 6" $
        getMonth 6 `shouldBe` "June"
      it "gives July on input 7" $
        getMonth 7 `shouldBe` "July"
      it "gives August on input 8" $
        getMonth 8 `shouldBe` "August"
      it "gives September on input 9" $
        getMonth 9 `shouldBe` "September"
      it "gives October on input 10" $
        getMonth 10 `shouldBe` "October"
      it "gives November on input 11" $
        getMonth 11 `shouldBe` "November"
      it "gives December on input 12" $
        getMonth 12 `shouldBe` "December"
      it "gives error on input 0" $
        evaluate (getMonth 0) `shouldThrow` errorCall "Unknown month"
      it "gives error on input 13" $
        evaluate (getMonth 13) `shouldThrow` errorCall "Unknown month"
      prop "gives same result as getMonth' for any number 1..12" $
        \m -> m>0 && m<13 ==> getMonth m `shouldBe` getMonth' m
    describe "getMonth'" $ do
      it "gives January on input 1" $
        getMonth' 1 `shouldBe` "January"
      it "gives February on input 2" $
        getMonth' 2 `shouldBe` "February"
      it "gives March on input 3" $
        getMonth' 3 `shouldBe` "March"
      it "gives April on input 4" $
        getMonth' 4 `shouldBe` "April"
      it "gives May on input 5" $
        getMonth' 5 `shouldBe` "May"
      it "gives June on input 6" $
        getMonth' 6 `shouldBe` "June"
      it "gives July on input 7" $
        getMonth' 7 `shouldBe` "July"
      it "gives August on input 8" $
        getMonth' 8 `shouldBe` "August"
      it "gives September on input 9" $
        getMonth' 9 `shouldBe` "September"
      it "gives October on input 10" $
        getMonth' 10 `shouldBe` "October"
      it "gives November on input 11" $
        getMonth' 11 `shouldBe` "November"
      it "gives December on input 12" $
        getMonth' 12 `shouldBe` "December"
      it "gives error on input 0" $
        evaluate (getMonth' 0) `shouldThrow` errorCall "Map.!: given key is not an element in the map"
      it "gives error on input 13" $
        evaluate (getMonth' 13) `shouldThrow` errorCall "Map.!: given key is not an element in the map"


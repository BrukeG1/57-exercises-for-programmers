module P20StateTaxSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P20StateTax hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "getTaxAmt" $ do
      prop "has the property of returning 0.005 times amount for wisconsin, eau clair" $
        \amt -> getTaxAmt "wisconsin" "eau clair" amt `shouldBe` amt * 0.005
      prop "has the property of returning 0.005 times amount for wn, eau clair" $
        \amt -> getTaxAmt "wn" "eau clair" amt `shouldBe` amt * 0.005
      prop "has the property of returning 0.004 times amount for wisconsin, dunn" $
        \amt -> getTaxAmt "wisconsin" "dunn" amt `shouldBe` amt * 0.004
      prop "has the property of returning 0.004 times amount for wn, dunn" $
        \amt -> getTaxAmt "wn" "dunn" amt `shouldBe` amt * 0.004
      prop "has the property of returning 0.08 times amount for illinois, (anything)" $
        \amt county-> getTaxAmt "illinois" county amt `shouldBe` amt * 0.08
      prop "has the property of returning 0.08 times amount for il, (anything)" $
        \amt county -> getTaxAmt "il" county amt `shouldBe` amt * 0.08
      prop "has the property of returning 0.2 times amount for oxfordshire, (anything)" $
        \amt area -> getTaxAmt "oxfordshire" area amt `shouldBe` amt * 0.2
      prop "has the property of returning 0.2 times amount for oxon, (anything)" $
        \amt area -> getTaxAmt "oxon" area amt `shouldBe` amt * 0.2
      prop "has the property of returning 0 for all other places" $
        \amt state county -> 
             state `notElem` [ "wisconsin"
                             , "wn"
                             , "illinois"
                             , "il"
                             , "oxfordshire"
                             , "oxon" ] ==> getTaxAmt state county amt `shouldBe` 0
    describe "getTaxAmt'" $ do
      prop "has the property of returning 0.005 times amount for wisconsin, eau clair" $
        \amt -> getTaxAmt' "wisconsin" "eau clair" amt `shouldBe` amt * 0.005
      prop "has the property of returning 0.005 times amount for wn, eau clair" $
        \amt -> getTaxAmt' "wn" "eau clair" amt `shouldBe` amt * 0.005
      prop "has the property of returning 0.004 times amount for wisconsin, dunn" $
        \amt -> getTaxAmt' "wisconsin" "dunn" amt `shouldBe` amt * 0.004
      prop "has the property of returning 0.004 times amount for wn, dunn" $
        \amt -> getTaxAmt' "wn" "dunn" amt `shouldBe` amt * 0.004
      prop "has the property of returning 0.08 times amount for illinois, (anything)" $
        \amt county-> getTaxAmt' "illinois" county amt `shouldBe` amt * 0.08
      prop "has the property of returning 0.08 times amount for il, (anything)" $
        \amt county -> getTaxAmt' "il" county amt `shouldBe` amt * 0.08
      prop "has the property of returning 0.2 times amount for oxfordshire, (anything)" $
        \amt area -> getTaxAmt' "oxfordshire" area amt `shouldBe` amt * 0.2
      prop "has the property of returning 0.2 times amount for oxon, (anything)" $
        \amt area -> getTaxAmt' "oxon" area amt `shouldBe` amt * 0.2
      prop "has the property of returning 0 for all other places" $
        \amt state county ->
             state `notElem` [ "wisconsin"
                             , "wn"
                             , "illinois"
                             , "il"
                             , "oxfordshire"
                             , "oxon" ] ==> getTaxAmt' state county amt `shouldBe` 0
      prop "has property of giving identical results to getTaxAmt for all inputs" $
        \a s c -> getTaxAmt a s c `shouldBe` getTaxAmt' a s c
    describe "getTaxRateFor" $ do
      it "gives zero rate for unknown state" $ do
        getTaxRateFor "blarb" taxLookup `shouldBe` (Rate 0)
      it "gives correct rate for illinois" $ do
        getTaxRateFor "illinois" taxLookup `shouldBe` (Rate 0.08)
      it "gives correct rate for illinois" $ do
        getTaxRateFor "illinois" taxLookup `shouldBe` (Rate 0.08)
      it "gives correct rate for dunn, wisconsin" $ do
        let counties = getTaxRateFor "wn" taxLookup
        getTaxRateFor "dunn" counties `shouldBe` (Rate 0.004)
      it "gives correct rate for bumblefuck, wisconsin" $ do
        let counties = getTaxRateFor "wisconsin" taxLookup
        getTaxRateFor "eau clair" counties `shouldBe` (Rate 0.005)
      it "gives correct rate for bumblefuck, wisconsin" $ do
        let counties = getTaxRateFor "wn" taxLookup
        getTaxRateFor "bumblefuck" counties `shouldBe` (Rate 0)

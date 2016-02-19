module P16DrivingAgeSpec (main,spec) where

import Test.Hspec
import P16DrivingAge hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "drivingMsg" $ do
    it "gives are of driving age for UK, 17" $
      drivingMsg "UK" 17 `shouldBe` "You are old enough to drive in UK"
    it "gives are not of driving age for UK, 16" $
      drivingMsg "UK" 16 `shouldBe` "You are not old enough to drive in UK"
  describe "countryStr" $ do
    it "gives UK for UK" $
      countryStr "UK" `shouldBe` "UK"
    it "gives US for US" $
      countryStr "US" `shouldBe` "US"
    it "gives UK and dont know msg for Belgium" $
      countryStr "Belgium" `shouldBe` "UK, don't know about Belgium"
  describe "knownCountry" $ do
    it "is true for UK" $
      knownCountry "UK" `shouldBe` True
    it "is true for US" $
      knownCountry "US" `shouldBe` True
    it "is false for Belgium" $
      knownCountry "Belgium" `shouldBe` False
  describe "isOfDrivingAge" $ do
    it "gives true for UK 17" $
      isOfDrivingAge "UK" 17 `shouldBe` True
    it "gives true for UK 18" $
      isOfDrivingAge "UK" 18 `shouldBe` True
    it "gives false for UK 16" $
      isOfDrivingAge "UK" 16 `shouldBe` False
    it "gives true for US 16" $
      isOfDrivingAge "US" 16 `shouldBe` True
    it "gives true for US 17" $
      isOfDrivingAge "US" 17 `shouldBe` True
    it "gives false for US 15" $
      isOfDrivingAge "US" 15 `shouldBe` False

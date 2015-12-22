module P25PwStrengthSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((==>))
import P25PwStrength hiding (main)
import Data.List (sort)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "passwordStrength" $ do
    it "identifies a very weak password" $ do
      passwordStrength "12345" `shouldBe` VeryWeak
    it "identifies a weak password" $ do
      passwordStrength "cats" `shouldBe` Weak
    it "identifies an average password" $ do
      passwordStrength "123cat" `shouldBe` Average
    it "identifies a strong password with 1 number" $ do
      passwordStrength "1ttffsse" `shouldBe` Strong
    it "identifies a strong password with 2 numbers" $ do
      passwordStrength "1ttf4fsse" `shouldBe` Strong
    it "identifies a very strong password" $ do
      passwordStrength "12ed12ed$$21" `shouldBe` VeryStrong
  describe "hasLetters" $ do
    it "is true when string contains letters" $ do
      hasLetters "yes12" `shouldBe` True
    it "is false when string does not contain letters" $ do
      hasLetters "5612" `shouldBe` False
    it "is false when string is empty" $ do
      hasLetters "" `shouldBe` False
  describe "manyLetters" $ do
    it "is true when string contains > 1 letters" $ do
      manyLetters "yes12" `shouldBe` True
    it "is false when string contains 1 letter" $ do
      manyLetters "56a12" `shouldBe` False
    it "is false when string contains no letters" $ do
      manyLetters "5612" `shouldBe` False
    it "is false when string is empty" $ do
      manyLetters "" `shouldBe` False
  describe "allLetters" $ do
    it "is true when string contains only letters" $ do
      allLetters "yesItDoes" `shouldBe` True
    it "is false when string contains no letters" $ do
      allLetters "5612" `shouldBe` False
    it "is false when string contains letters and non-letters" $ do
      allLetters "ee5612" `shouldBe` False
    it "is true when string is empty" $ do
      allLetters "" `shouldBe` True
  describe "hasNumbers" $ do
    it "is true when string contains numbers" $ do
      hasNumbers "yes12" `shouldBe` True
    it "is false when string does not contain numbers" $ do
      hasNumbers "piss" `shouldBe` False
    it "is false when string is empty" $ do
      hasNumbers "" `shouldBe` False
  describe "manyNumbers" $ do
    it "is true when string contains > 1 numbers" $ do
      manyNumbers "yes12" `shouldBe` True
    it "is false when string contains 1 number" $ do
      manyNumbers "erk2q" `shouldBe` False
    it "is false when string contains no numbers" $ do
      manyNumbers "pokey" `shouldBe` False
    it "is false when string is empty" $ do
      manyNumbers "" `shouldBe` False
  describe "allNumbers" $ do
    it "is true when string contains only numbers" $ do
      allNumbers "19898298" `shouldBe` True
    it "is false when string contains no numbers" $ do
      allNumbers "raga" `shouldBe` False
    it "is false when string contains numbers and non-numbers" $ do
      allNumbers "ee5612" `shouldBe` False
    it "is true when string is empty" $ do
      allNumbers "" `shouldBe` True
  describe "hasSpecials" $ do
    it "is true when string contains punctuation" $ do
      hasSpecials "yes12'" `shouldBe` True
    it "is true when string contains a symbol" $ do
      hasSpecials "yes12♥" `shouldBe` True
    it "is false when string does not contain punctuation" $ do
      hasSpecials "piss" `shouldBe` False
    it "is false when string is empty" $ do
      hasSpecials "" `shouldBe` False
  describe "manySpecials" $ do
    it "is true when string contains > 1 punctiuation chars" $ do
      manySpecials "yes12?'" `shouldBe` True
    it "is false when string contains 1 punctuation char" $ do
      manySpecials "erk-2q" `shouldBe` False
    it "is true when string contains > 1 symbol" $ do
      manySpecials "yes12♥♻" `shouldBe` True
    it "is false when string contains 1 symbol" $ do
      manySpecials "erk♥2q" `shouldBe` False
    it "is false when string contains no punctuation chars or symbols" $ do
      manySpecials "pokey" `shouldBe` False
    it "is false when string is empty" $ do
      manySpecials "" `shouldBe` False

module P37PasswordGenSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P37PasswordGen hiding (main)
import Data.Char(isLetter, isNumber)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mkPass" $ do
    it "creates correct length passwords" $ do
      p <- mkPass 3 1 1
      length p `shouldBe` 3
    it "creates passwords containing correct number of letters" $ do
      p <- mkPass 3 1 1
      (length $ filter isLetter p) `shouldBe` 1
    it "creates passwords containing correct number of symbols" $ do
      p <- mkPass 6 2 1
      (length $ filter (\x -> (not $ isNumber x) && ( not $ isLetter x)) p) `shouldBe` 2
    it "creates passwords containing correct number of numbers" $ do
      p <- mkPass 16 2 9
      (length $ filter isNumber p) `shouldBe` 9
    it "creates different passwords each time" $ do
      p <- mkPass 16 2 9
      p' <- mkPass 16 2 9
      p `shouldSatisfy` (/=) p'

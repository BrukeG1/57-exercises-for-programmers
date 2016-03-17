module P45WordFinderSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P45WordFinder hiding (main)
import Data.List (isInfixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "replace" $ do
      it "given a string, a word to replace and the word to replace it with, return a tuple of unchanged string and replacement count of 0 if word to replace not present in original string" $ do
        replace "monkeys" "dogs" "\"cats\" and dogs" `shouldBe` ("\"cats\" and dogs",0)
      it "given a string, a word to replace and the word to replace it with, return a tuple of changed string and replacement count of 1 if the word is replaced once" $ do
        replace "cats" "dogs" "\"cats\" and dogs" `shouldBe` ("\"dogs\" and dogs",1)
      prop "has the property of never leaving the string to be removed in the target string" $
        \xs ys -> (not (null xs || null ys)) ==> do
          let zs = replace xs "" ys
          xs `isInfixOf` fst zs `shouldBe` False
      prop "has the property that replacing a string with itself has no effect on the target string" $
        \xs ys -> fst (replace xs xs ys) == ys

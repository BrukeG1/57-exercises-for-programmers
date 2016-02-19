module P35WinnerSpec (main,spec) where

import Test.Hspec
import P35Winner hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deleteAt" $ do
    it "deletes first elt of a 2 elt list" $ do
      deleteAt 0 ["a","b"] `shouldBe` ["b"]
    it "deletes second elt of a 2 elt list" $ do
      deleteAt 1 ["a","b"] `shouldBe` ["a"]

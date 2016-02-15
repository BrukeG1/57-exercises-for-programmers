module P08PizzaSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P08Pizza hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "formatSlices" $ do
    it "shows right message for no slices" $ do
      formatSlices 0 `shouldBe` " So nobody gets a slice of pizza at all. Boo! And there was"
    it "shows right message for 1 slices" $ do
      formatSlices 1 `shouldBe` " So each person gets a single slice of pizza, with"
    it "shows right message for 2 slices" $ do
      formatSlices 2 `shouldBe` " So each person gets 2 slices of pizza, with"
  describe "formatPizzas" $ do
    it "shows right message for no pizzas" $ do
      formatPizzas 0 `shouldBe` "There was no pizza. Boo!"
    it "shows right message for 1 pizzas" $ do
      formatPizzas 1 `shouldBe` "There was 1 pizza."
    it "shows right message for 2 pizzas" $ do
      formatPizzas 2 `shouldBe` "There were 2 pizzas."
  describe "formatLeftovers" $ do
    it "shows right message for no slices" $ do
      formatLeftovers 0 `shouldBe` " no slices left over."
    it "shows right message for 1 slices" $ do
      formatLeftovers 1 `shouldBe` " one slice left over."
    it "shows right message for 2 slices" $ do
      formatLeftovers 2 `shouldBe` " 2 slices left over."


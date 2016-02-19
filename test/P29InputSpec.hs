module P29InputSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P29Input hiding (main)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "expectedReturnOn" $ do
    it "calculates the book example correctly" $ do
      expectedReturnOn 4 `shouldBe` 18

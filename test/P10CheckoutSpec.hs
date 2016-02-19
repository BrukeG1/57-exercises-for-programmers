module P10CheckoutSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P10Checkout hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "getTax" $ do
    prop "works out tax correctly" $
      \x -> getTax x `shouldReturn` x*0.055
  describe "totalize" $ do
    it "should work out the subtotal correctly" $ do
      totalize 15 (9,9,9) `shouldReturn` 96.0
    it "should work out the subtotal correctly with decimals involved" $ do
      totalize 15.01 (9,9.01,9) `shouldReturn` 96.1

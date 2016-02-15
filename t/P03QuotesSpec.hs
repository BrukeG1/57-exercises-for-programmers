module P03QuotesSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P03Quotes hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "mkQuote" $ do
    it "correctly quotes when given x and y as who and quote" $ do
      mkQuote "x" "y" == "x says, \"y\""


module P02NumCharsSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P02NumChars hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "validate" $ do
    it "gives Nothing for an empty string" $ do
      validate "" == Nothing
    it "gives correct result for 'e' string" $ do
      validate "e" == Just "'e' has 1 character"
    it "gives correct result for 'ed' string" $ do
      validate "ed" == Just "'ed' has 2 characters"

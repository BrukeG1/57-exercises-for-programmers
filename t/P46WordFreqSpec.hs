module P46WordFreqSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P46WordFreq hiding (main)
 
 
main :: IO ()
main = hspec spec

spec = do
  describe "main" $ do
    it "is not implemented yet" $ 
      pending

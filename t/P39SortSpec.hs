module P39SortSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P39Sort hiding (main)
 
 
main :: IO ()
main = hspec spec

spec = do
  describe "main" $ do
    it "is not implemented yet" $ 
      pending

module P42ParsingSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P42Parsing hiding (main)
 
 
main :: IO ()
main = hspec spec

spec = do
  describe "main" $ do
    it "is not implemented yet" $ 
      pending

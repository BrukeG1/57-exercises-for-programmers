module P37PasswordGenSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P37PasswordGen hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "main" $ do
    it "is not implemented yet" $ 
      pending

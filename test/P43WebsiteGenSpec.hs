module P43WebsiteGenSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P43WebsiteGen hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "indexHTML" $ do
      it "pends" pending


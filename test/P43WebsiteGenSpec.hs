module P43WebsiteGenSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P43WebsiteGen hiding (main)
import Data.List (isInfixOf)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "indexHTML" $ do
      it "sets title correctly" $ do
        "<title>mytitle" `isInfixOf` indexHTML meta
      it "sets author correctly" $ do
        "<meta name=\"author\" value=\"myauthor\">" `isInfixOf` indexHTML meta

meta = Meta "mytitle" "myauthor"

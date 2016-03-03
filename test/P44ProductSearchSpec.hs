module P44ProductSearchSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P44ProductSearch hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "findByName" $ do
      it "returns the catalogue entry for a widget" $ do
        findByName "Widget" `shouldReturn` [Product {name = "Widget", price = 25.0, quantity = 5}]
      it "returns the catalogue entry for a widget, with part of the name" $ do
        findByName "Wid" `shouldReturn` [Product {name = "Widget", price = 25.0, quantity = 5}]
      it "returns the catalogue entry for a widget, with part of the name, case insensitively" $ do
        findByName "wid" `shouldReturn` [Product {name = "Widget", price = 25.0, quantity = 5}]
      it "returns an empty list for an unsuccessful search" $ do
        findByName "jkdajdksadksajkd" `shouldReturn` []


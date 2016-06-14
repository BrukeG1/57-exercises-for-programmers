{-# LANGUAGE OverloadedStrings #-}
module P50MovieSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P50Movie hiding (main)
import Data.Text (unpack)
import Control.Lens ((^.),(^?))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    it "Won't be implemented" $ do
      pendingWith "Due to RottenTomatoes being dicks about their API"

module P23CarTroublesSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck((==>))
import P23CarTroubles hiding (main)
--import qualified Test.IOSpec as IO

emptyList :: [Int]
emptyList = []

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "nextStep" $ do
      it "gives the message contained in a leaf" $
        nextStep (Leaf "x") `shouldReturn` "x"
      -- TODO: IO testing? IOSpec? 
      -- -- it "prints y/n and the next msg when given a branch" $

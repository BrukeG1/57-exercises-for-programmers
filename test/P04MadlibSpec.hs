module P04MadlibSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P04Madlib hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "mkStory" $ do
    it "creates a story containing the noun, verb, adjective and adverb provided" $ do
      s <- mkStory "shuffle" "smelly" "arrays" "unconsciously"
      s `shouldSatisfy` (not . null)
    it "creates a story containing the noun, verb, adjective and adverb provided" $ do
      s <- mkStory "shuffle" "smelly" "arrays" "unconsciously"
      s `shouldContain` "shuffle"
    it "creates a story containing the noun, verb, adjective and adverb provided" $ do
      s <- mkStory "shuffle" "smelly" "arrays" "unconsciously"
      s `shouldContain` "smelly"
    it "creates a story containing the noun, verb, adjective and adverb provided" $ do
      s <- mkStory "shuffle" "smelly" "arrays" "unconsciously"
      s `shouldContain` "arrays"

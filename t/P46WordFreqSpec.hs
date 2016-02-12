module P46WordFreqSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P46WordFreq hiding (main)
 
testWds :: [String]
testWds = [ "badger"
      , "badger"
      , "badger"
      , "badger"
      , "badger"
      , "badger"
      , "badger"
      , "mushroom"
      , "mushroom"
      , "plinth"
      ]
 
main :: IO ()
main = hspec spec

spec = do
  describe "freqWds" $ do
    it "counts the occurences of badger correctly" $ do
      (fst . head $ freqWds testWds) == "badger"
      && (snd . head $ freqWds testWds) == 7
    it "counts the occurences of mushroom correctly" $ do
      (fst . head . drop 1 $ freqWds testWds) == "mushroom"
      && (snd . head . drop 1 $ freqWds testWds) == 2
    it "counts the occurences of plinth correctly" $ do
      (fst . last $ freqWds testWds) == "plinth"
      && (snd . last $ freqWds testWds) == 1
  describe "freqWds'" $ do
    it "counts the occurences of badger correctly" $ do
      (fst . head $ freqWds' testWds) == "badger"
      && (snd . head $ freqWds' testWds) == 7
    it "counts the occurences of mushroom correctly" $ do
      (fst . head . drop 1 $ freqWds' testWds) == "mushroom"
      && (snd . head . drop 1 $ freqWds' testWds) == 2
    it "counts the occurences of plinth correctly" $ do
      (fst . last $ freqWds' testWds) == "plinth"
      && (snd . last $ freqWds' testWds) == 1
    prop "has the property of giving the same result as freqWds for all lists" $
      \xs -> freqWds' xs == freqWds xs

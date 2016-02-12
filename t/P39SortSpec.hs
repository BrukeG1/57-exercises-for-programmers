module P39SortSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P39Sort hiding (main)
import Data.Map as M
 
testData :: M.Map String [String]
testData =
    M.fromList [ ("Johnson",    ["John",      "Manager",           "2016-12-31"])
               , ("Xiong",      ["Tou",       "Software Engineer", "2016-10-05"])
               , ("Michaelson", ["Michela",   "District Manager",  "2015-12-19"])
               , ("Jacobson",   ["Jake",      "Programmer",        ""])
               , ("Jackson",    ["Jacquelyn", "DBA",               ""])
               , ("Weber",      ["Sally",     "Web Develeoper",    "2015-12-18"])
               , ("Archimedes", ["Xerxes",    "Tester",            "2016-11-18"])
               ]
 
main :: IO ()
main = hspec spec

spec = do
  describe "lastNameSort" $ do
    it "correctly sorts test data" $ do
      (fst . head $ lastNameSort testData) == "Archimedes"
      && (fst . last $ lastNameSort testData) == "Xiong"
  describe "firstNameSort" $ do
    it "correctly sorts test data" $ do
      (fst . head $ firstNameSort testData) == "Jackson"
      && (fst . last $ firstNameSort testData) == "Archimedes"
  describe "jobTitleSort" $ do
    it "correctly sorts test data" $ do
      (fst . head $ jobTitleSort testData) == "Jackson"
      && (fst . last $ jobTitleSort testData) == "Weber"
  describe "sepDateSort" $ do
    it "correctly sorts test data" $ do
      (fst . head $ sepDateSort testData) == "Jackson"
      && (fst . last $ sepDateSort testData) == "Johnson"

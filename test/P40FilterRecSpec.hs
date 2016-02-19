module P40FilterRecSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P40FilterRec hiding (main)
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Calendar

testData :: M.Map String [String]
testData =
    M.fromList [ ("Johnson",    ["John",      "Manager",           "2016-02-11"])
               , ("Xiong",      ["Tou",       "Software Engineer", "2016-10-05"])
               , ("Michaelson", ["Michela",   "District Manager",  "2015-02-19"])
               , ("Jacobson",   ["Jake",      "Programmer",        ""])
               , ("Jackson",    ["Jacquelyn", "DBA",               ""])
               , ("Weber",      ["Sally",     "Web Develeoper",    "2015-12-18"])
               , ("Archimedes", ["Xerxes",    "Tester",            "2016-01-18"])
               ]

main :: IO ()
main = hspec spec

spec = do
  describe "filterByName" $ do
    it "filters on a string that is present" $ do
      2 == (length . M.toList $ filterByName "jac" testData)
    it "filters on a string that is not present" $ do
      null . M.toList $ filterByName "zxr" testData
  describe "filterByPosn" $ do
    it "filters on a job that is present" $ do
      1 == (length . M.toList $ filterByPosn "programmer" testData)
    it "filters on a job that is not present" $ do
      null . M.toList $ filterByPosn "zxr" testData
  describe "filterBySepDate" $ do
    it "filters on a date that has results within 6 months" $ do
      1 == (length . M.toList $ filterBySepDate (UTCTime (fromGregorian 2016 2 12) 0) testData)
    it "filters on a date that has no results within 6 months" $ do
      null . M.toList $ filterBySepDate (UTCTime (fromGregorian 2001 1 1) 0) testData

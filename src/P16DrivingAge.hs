module P16DrivingAge where

-- TODO: tests!!

import qualified Data.Map as M
import Library

type Country = String

defaultDrivingAge :: Int
defaultDrivingAge = 17

defaultDrivingCountry :: Country
defaultDrivingCountry = "UK"

drivingAges :: M.Map Country Int
drivingAges = M.fromList [("UK",17), ("US",16)]

main :: IO ()
main = do
    age <- promptNonNegInt "How old are you: "
    country <- promptS "Your country (eg. DE, US, etc default UK): "
    putStrLn $ drivingMsg country age 

drivingMsg :: Country -> Int -> String
drivingMsg country age =
       "You " ++ (if isOfDrivingAge country age then "are" else "are not")
       ++ " old enough to drive in " ++ countryStr country

countryStr :: Country -> String
countryStr country =
    if knownCountry country 
      then country
      else defaultDrivingCountry ++ ", don't know about " ++ country

knownCountry :: Country -> Bool
knownCountry country =
    M.member country drivingAges

isOfDrivingAge :: Country -> Int -> Bool
isOfDrivingAge country age = 
    age >= countryDrivingAge
  where
    countryDrivingAge = M.findWithDefault defaultDrivingAge country drivingAges

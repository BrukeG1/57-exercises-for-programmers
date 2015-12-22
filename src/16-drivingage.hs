import qualified Data.Map as M

type Country = String

defaultDrivingAge :: Int
defaultDrivingAge     = 17

defaultDrivingCountry :: Country
defaultDrivingCountry = "UK"

drivingAges :: M.Map Country Int
drivingAges = M.fromList [("UK",17), ("US",16)]

main :: IO ()
main = do
    age <- promptN "How old are you: "
    country <- promptS "Your country (eg. DE, US, etc default UK): "
    putStrLn $ "You " ++ (if isOfDrivingAge country age then "are" else "are not") 
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

promptS :: String -> IO String
promptS m = do
    putStr m
    getLine

promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    x <- readLn -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x

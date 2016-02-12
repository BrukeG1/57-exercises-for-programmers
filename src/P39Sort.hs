module P39Sort where
import Data.Map as M
import Text.Printf(printf)
import Data.Function (on)
import Data.List (sortBy)
import Control.Exception

main :: IO ()
main = do
    putStrLn "0: Last name"
    putStrLn "1: First name"
    putStrLn "2: Job title"
    putStrLn "3: Sep. date"
    f <- promptN "Choose sort field [0..3]: "
    putStrLn . mkTbl $ case f of
      0 -> lastNameSort theData
      1 -> firstNameSort theData
      2 -> jobTitleSort theData
      3 -> sepDateSort theData
      _ -> error "Invalid choice encountered"

theData :: M.Map String [String]
theData =
    M.fromList [ ("Johnson",    ["John",      "Manager",           "2016-12-31"])
               , ("Xiong",      ["Tou",       "Software Engineer", "2016-10-05"])
               , ("Michaelson", ["Michela",   "District Manager",  "2015-12-19"])
               , ("Jacobson",   ["Jake",      "Programmer",        ""])
               , ("Jackson",    ["Jacquelyn", "DBA",               ""])
               , ("Weber",      ["Sally",     "Web Develeoper",    "2015-12-18"])
               , ("Archimedes", ["Xerxes",    "Tester",            "2016-11-18"])
               ]

lastNameSort :: M.Map String [String] -> [(String,[String])]
lastNameSort = M.assocs

firstNameSort :: M.Map String [String] -> [(String,[String])]
firstNameSort = sortWithField 0

jobTitleSort :: M.Map String [String] -> [(String,[String])]
jobTitleSort = sortWithField 1

sepDateSort :: M.Map String [String] -> [(String,[String])]
sepDateSort = sortWithField 2

sortWithField :: Int -> M.Map String [String] -> [(String,[String])]
sortWithField n =
    sortBy (compare `on` ((!!n) . snd)) . M.toList 

mkTbl :: [(String,[String])] -> String
mkTbl xs =
    "Name                | Position          | Separation date\n"
    ++ "--------------------|-------------------|----------------\n"
    ++ concatMap mkRow xs

mkRow :: (String,[String]) -> String
mkRow (lname,fname:job:sepdate:[]) =
    name ++ jtitle ++ sdat
  where
    name   = printf "%-20s" (fname ++ " " ++ lname)
    jtitle = printf "| %-18s" job
    sdat   = printf "| %-10s\n" sepdate
mkRow _ = ""

promptN :: String -> IO Int
promptN m = do
    putStr m
    x <- readLn `catch` except
    if x<0 || x>3
      then do
        putStrLn "Expecting a number from 0..3"
        promptN m
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptN m


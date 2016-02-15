module P42Parsing where

import Text.Printf(printf)
import Data.List(sortBy)
import Data.List.Split(splitOn)
import Data.Function(on)
import Data.Vector (toList)
import Data.Csv
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
    putStrLn "Normal sort\n"
    putStrLn tblHeader
    putStrLn $ mkTbl theFile
    putStrLn "Salary sort\n"
    putStrLn tblHeader
    putStrLn $ mkSortTbl 2 theFile
    putStrLn "Salary sort using Data.Csv\n"
    putStrLn tblHeader
    putStrLn $ mkSortTbl' 2 theFile

tblHeader :: String
tblHeader =
       "Last     First    Salary\n"
    ++ "------------------------"

mkSortTbl :: Int -> [String] -> String
mkSortTbl n xs =
      concatMap mkRow . sortBy (flip compare `on` (!!n)) $ map toFields xs

mkSortTbl' :: Int -> [String] -> String
mkSortTbl' n xs =
      concatMap mkRow . sortBy (flip compare `on` (!!n)) $ toFieldRows xs

mkTbl :: [String] -> String
mkTbl =
    concatMap (mkRow . toFields)

mkRow :: [String] -> String
mkRow fs =
    l ++ f ++ s ++ "\n"
  where
    l = printf "%-9s" $ lname fs
    f = printf "%-9s" $ fname fs
    s = printf "%-6s" $ salary fs

toFields :: String -> [String]
toFields = splitOn ","

toFieldRows :: [String] -> [[String]]
toFieldRows xs = 
    case xs' of
      Left e -> error e
      Right xs'' -> toList xs''
  where
    xs' = decode NoHeader . B.pack $ unlines xs

fname :: [String] -> String
fname = (!!1)

lname :: [String] -> String
lname = (!!0)

salary :: [String] -> String
salary = (!!2)

theFile :: [String]
theFile = [ "Ling,Mai,55900"
          , "Johnson,Jim,56500"
          , "Jones,Aaron,46000"
          , "Jones,Chris,34500"
          , "Swift,Geoffrey,14200"
          , "Xiong,Fong,65000"
          , "Zarnecki,Sabrina,51500"
          ]

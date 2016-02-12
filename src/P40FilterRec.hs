module P40FilterRec where
import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Time
import Data.List.Split(splitOn)

main :: IO ()
main = do
    putStr "Search string: "
    nm <- getLine
    putStrLn . concatMap mkRow . M.toList $ filterByName nm theData
    c <- getCurrentTime
    -- putStrLn . concatMap mkRow . M.toList $ filterBySepDate c theData
    -- You can compose filters like this (search for 'e'):
    putStrLn "People who left more than 6 months ago"
    putStrLn . concatMap mkRow . M.toList . filterBySepDate c . filterByPosn nm $ filterByName nm theData

-- TODO: Refactor to use M.Map
mkRow :: (String, [String]) -> String
mkRow x =
    fst x ++ " " ++ unwords (snd x) ++ "\n"

theData :: M.Map String [String]
theData =
    M.fromList [ ("Johnson",    ["John",      "Manager",           "2016-02-11"])
               , ("Xiong",      ["Tou",       "Software Engineer", "2016-10-05"])
               , ("Michaelson", ["Michela",   "District Manager",  "2015-02-19"])
               , ("Jacobson",   ["Jake",      "Programmer",        ""])
               , ("Jackson",    ["Jacquelyn", "DBA",               ""])
               , ("Weber",      ["Sally",     "Web Develeoper",    "2015-12-18"])
               , ("Archimedes", ["Xerxes",    "Tester",            "2016-01-18"])
               ]

filterByName :: String -> M.Map String [String] -> M.Map String [String]
filterByName nm =
    M.filterWithKey (\fn xs -> nm' `isInfixOf` lc fn || nm' `isInfixOf` lc (head xs))
  where
    nm' = lc nm

filterByPosn :: String -> M.Map String [String] -> M.Map String [String]
filterByPosn pn =
    M.filterWithKey (\_ xs -> lc pn `isInfixOf` lc (xs !! 1) )

filterBySepDate :: UTCTime -> M.Map String [String] -> M.Map String [String]
filterBySepDate now =
    M.filterWithKey (\_ xs -> (not . null $ xs !! 2) && (xs !! 2) `sixOrMoreMonthsBefore`  now)

lc :: String -> String
lc = map toLower

sixOrMoreMonthsBefore :: String -> UTCTime -> Bool
a `sixOrMoreMonthsBefore` b =
    diffDays (utctDay b) (iso8601toDate a) > 6 * 30

iso8601toDate :: String -> Day
iso8601toDate is =
    fromGregorian y m d
  where
    (y,m,d) = to3tuple . take 3 $ splitOn "-" is

to3tuple :: [String] -> (Integer,Int,Int)
to3tuple (a:b:c:[]) = (read a, read b, read c)
to3tuple _          = error "require a 3 elt list to construct a 3 elt tuple"

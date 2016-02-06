module P46WordFreq where

import qualified Data.Map as M
import Data.Function(on)
import Data.List(sortBy)
import Text.Printf(printf)

-- TODO: Test on shakespeare, bar chart, write in another language, tests

file :: String
file = "../../../barncamp-signing-annotated.txt"

main :: IO ()
main = do
    xs <- readFile file
    putStrLn $ concatMap mkRow . countWds $ words xs

wds :: [String]
wds = [ "badger"
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

countWds :: [String] -> [(String,Int)]
countWds ws =
    sortBy (flip compare `on` snd) . M.toList $
      foldr (\w m -> if M.member w m
                     then M.insert w ((M.!)m w + 1) m
                     else M.insert w 1 m) M.empty ws

mkRow :: (String, Int) -> String
mkRow (w,c) = printf "%-25s: " (take 25 w) ++ show c ++ "\n"

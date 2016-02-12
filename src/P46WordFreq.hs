module P46WordFreq where

import qualified Data.Map as M
import Data.Function(on)
import Data.List(sortBy,sort, group)
import Text.Printf(printf)
import Control.Arrow ((&&&))
-- TODO: Test on shakespeare, bar chart, write in another language, tests

file :: String
--file = "../../../barncamp-signing-annotated.txt"
file = "shakespeare.txt"

main :: IO ()
main = do
    xs <- readFile file
    putStrLn $ concatMap mkRow . freqWds $ words xs

freqWds :: Ord a => [a] -> [(a,Int)]
freqWds =
  sortBy (flip compare `on` snd) . map (head &&& length) . group . sort

freqWds' :: [String] -> [(String,Int)]
freqWds' ws =
    sortBy (flip compare `on` snd) . M.toList $
      foldr (\w m -> if M.member w m
                     then M.insert w ((M.!)m w + 1) m
                     else M.insert w 1 m) M.empty ws

mkRow :: (String, Int) -> String
mkRow (w,c) = printf "%-25s: " (take 25 w) ++ show c ++ "\n"

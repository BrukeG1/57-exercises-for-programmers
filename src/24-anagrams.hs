module Anagram24 where
import Data.Char (isSpace)

main :: IO ()
main = do
    putStrLn "Enter 2 strings and I'll tell you if they are anagrams:"
    putStr "First: "
    xs <- getLine
    putStr "Second: "
    ys <- getLine
    putStrLn $ "'" ++ xs ++ "' and '" ++ ys ++ if xs `isAnagram` ys
                                               then "' are anagrams."
                                               else "' are not anagrams."

isAnagram :: String -> String -> Bool
isAnagram xs ys =
    cleanxs `sameLength` cleanys && sortedxs == sortedys
  where 
    sortedxs = mergeSort (<=) cleanxs
    sortedys = mergeSort (<=) cleanys
    cleanxs  = filter (not . isSpace) xs
    cleanys  = filter (not . isSpace) ys

sameLength :: [a] -> [a] -> Bool
sameLength xs ys =
    length xs == length ys

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge p xs@(x:xt) ys@(y:yt) | x `p` y = x : merge p xt ys
                            | otherwise  = y : merge p xs yt

split :: [a] -> ([a],[a])
split (x:y:zs) =
    (x:xs,y:ys)
  where 
    (xs,ys) = split zs

split [x] = ([x],[])
split []  = ([],[])

mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ []  = []
mergeSort _ [x] = [x]
mergeSort p xs  =
    merge p (mergeSort p as) (mergeSort p bs)
  where
    (as,bs) = split xs

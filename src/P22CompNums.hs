module P22CompNums where

import Library

main :: IO ()
main = do
    x <- promptNonNegInt "How many integers will you enter? "
    is <- getUniqInts x []
    putStrLn $ case maxInt is of
                  Nothing -> "No largest integer found"
                  Just n  -> "The largest integer was: " ++ show n

-- Get some number of unique integers from the user
getUniqInts :: Int -> [Int] -> IO [Int]
getUniqInts 0 xs = return xs
getUniqInts n xs = do
  i <- promptUniq "Enter an unique integer: " xs
  getUniqInts (n-1) (i : xs)

-- Makes calling max a bit nicer
maxInt ::  [Int] -> Maybe Int
maxInt = max' Nothing

-- Get the maximum from a list of Ords
max' :: (Ord a, Eq a) => Maybe a -> [a] -> Maybe a
max' n []              = n
max' Nothing (x:xs)    = max' (Just x) xs
max' m@(Just n) (x:xs) = if x > n
                         then max' (Just x) xs
                         else max' m xs

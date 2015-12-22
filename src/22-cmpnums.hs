main :: IO ()
main = do
    putStr "How many integers will you enter? "
    x <- readLn
    if x < 0
      then error "You can't enter fewer than zero integers!"
      else do 
        is <- getUniqInts x []
        putStrLn $ case maxInt is of
                    Nothing -> "No largest integer found"
                    Just n  -> "The largest integer was: " ++ show n

-- get a number of unique integers from the user
getUniqInts :: Int -> [Int] -> IO [Int]
getUniqInts 0 xs = return xs
getUniqInts n xs = do
  i <- prompt "Enter an unique integer: " xs
  getUniqInts (n-1) (i : xs)

-- A custom prompt implementation. Take a message and a list of already seen entries
-- and keep asking for numbers until you get one that is not in the list already
prompt :: (Eq a, Read a) => String -> [a] -> IO a
prompt m xs = do
    putStr m
    x <- readLn -- catch parse error to recover
    if x `elem` xs
      then do
        putStrLn "Integers must be unique!"
        prompt m xs
      else
        return x

-- Makes calling max a bit nicer
maxInt :: Ord a => [a] -> Maybe a
maxInt = max' Nothing

-- Get the maximum from a list of Ords
max' :: Ord a => Maybe a -> [a] -> Maybe a
max' Nothing []        = Nothing
max' n []              = n
max' Nothing (x:xs)    = max' (Just x) xs
max' m@(Just n) (x:xs) = if x > n
                         then max' (Just x) xs
                         else max' m xs

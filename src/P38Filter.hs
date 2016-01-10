module P38Filter where
import Control.Exception (catch, IOException)

main :: IO ()
main = do
    xs <- promptXs "Enter a list of numbers separated by spaces: "
    let evens = filt evn xs
    putStrLn $ "The even numbers are: " ++ show evens

filt :: (a -> Bool) -> [a] -> [a]
filt p = foldr (\x xs -> if p x then x:xs else xs) []

-- originally did:
-- filt _ [] = []
-- filt p (x:xs) = if p x then x : filt p xs else filt p xs

-- not used, but cool
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

evn :: Int -> Bool
evn n = n `mod` 2 == 0

od :: Int -> Bool
od = not . evn

promptXs :: String -> IO [Int]
promptXs m = do
    putStr m
    x <- getLine
    return (map read $ words x) `catch` except
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptXs m

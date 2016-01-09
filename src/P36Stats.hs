module P36Stats where
import Control.Exception (IOException, catch)
import Text.Printf (printf)

-- TODO (wontfix) read numbers from a file -- you'd just xs <- readFile "lalala.txt"

main :: IO ()
main = do
  xs <- promptXs "Enter space separated list of numbers to analyze (eg. 1 2 3 4): "
  putStrLn $ "Average: " ++ show (avg xs)
  putStrLn $ "Maximum: " ++ show (maximum xs)
  putStrLn $ "Minimum: " ++ show (minimum xs)
  putStrLn $ "Sample standard deviation (rounded): " ++ show (sd xs)
  putStrLn $ "Sample standard deviation (unrounded): " ++ show (sampSD xs)
  putStrLn $ "Population standard deviation (unrounded): " ++ show (popSD xs)

-- Round sample SD to 2 decimal places
sd :: [Double] -> Double
sd = read . printf "%0.2f" . sampSD

-- This is the way to calculate sd for a whole population, but comes out
-- at ~408.25 which makes me think that:
-- 1. The book wanted us to calculate this way, but there is a misprint
--    (400.25 instead of ~408.25) and the instructions are incorrect
-- 2. The book wanted us to use population sd but there is a really odd misprint
--    (400.25 instead of 353.55)
sampSD :: [Double] -> Double
sampSD xs = sqrt $ variance xs nMinus1Avg

-- This is the calculation of sd given in the book, but comes 
-- out at ~353.553
popSD :: [Double] -> Double
popSD xs = sqrt $ variance xs avg

-- You pass in an average function which is the bit that varies for
-- population and sample standard deviation
variance :: [Double] -> ([Double] -> Double) -> Double
variance xs avgF =
    if length xs < 2
      then 0
      else avgF $ map dM2 xs
  where
    dM2 x = (x - mean) ** 2
    mean  = avg xs

-- Calculate the mean
avg :: [Double] -> Double
avg [] = 0
avg xs = sum xs / fromIntegral (length xs)

-- Calculate the mean for n-1 elts
nMinus1Avg :: [Double] -> Double
nMinus1Avg []  = 0
nMinus1Avg [_] = 0
nMinus1Avg xs  = sum xs / fromIntegral (length xs - 1)


promptXs :: String -> IO [Double]
promptXs m = do
    putStr m
    x <- getLine
    return (map read $ words x) `catch` except
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptXs m

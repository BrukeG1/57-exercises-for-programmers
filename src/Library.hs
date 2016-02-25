module Library where
-- reusable function used in more than one place
import Control.Exception (catch, IOException)
import Control.Monad (forM)
import System.Random (randomRIO)
import Data.Array.IO (writeArray, IOArray, readArray, newListArray)
import System.IO (hSetEcho,stdin,hFlush, stdout)
import Data.Char (toUpper, toLower)
import Text.Printf (printf)

-- Randomly shuffle a list
-- /O(N)/ from: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n' =  newListArray (1,n')

uc :: String -> String
uc = map toUpper

lc :: String -> String
lc = map toLower

showD :: Float -> String
showD = printf "%.2f"

showD8 :: Float -> String
showD8 = printf "%8.2f"

-- Prompt for a string, with a message
promptS :: String -> IO String
promptS m = putStr m >> hFlush stdout >> getLine

-- Prompt for a password (dont echo input)
promptP :: String -> IO String
promptP m = do
    putStr m
    hFlush stdout
    hSetEcho stdin False
    pass <- getLine
    hSetEcho stdin True
    putStrLn ""
    return pass

-- Prompts for a number with message M
-- Warns and restarts on no parse
promptNum :: (Num a, Ord a, Read a) => String -> IO a
promptNum m = do
    putStr m
    hFlush stdout
    x <- readLn `catch` except
    return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptNum m

-- Prompt for a non-negative number, with message
-- Warns and restarts on no parse
-- Warns and restarts for numbers < 0
promptNonNegNum :: (Num a, Ord a, Read a) => String -> IO a
promptNonNegNum m = do
    putStr m
    hFlush stdout
    x <- readLn `catch` except
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptNonNegNum m
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptNonNegNum m

-- Prompt for a non-negative float, with message
promptNonNegFloat :: String -> IO Float
promptNonNegFloat = promptNonNegNum

-- Prompt for a non-negative Int, with message
promptNonNegInt :: String -> IO Int
promptNonNegInt = promptNonNegNum

promptMonth :: String -> IO Int
promptMonth m = do
  mon <- promptNum m
  if mon < 1 || mon > 12
    then do
      putStrLn "Months are between 1 (Jan) and 12 (Dec)"
      promptMonth m
    else return mon


-- Take a message and a list of already seen entries
-- and keep asking for numbers until you get one that
-- is not in the list already.
-- Show error and repeat on parse problem.
promptUniq :: (Eq a, Read a) => String -> [a] -> IO a
promptUniq m xs = do
    putStr m
    hFlush stdout
    x <- readLn  `catch` except
    if x `elem` xs
      then do
        putStrLn "Items must be unique"
        promptUniq m xs
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse input. Error was: " ++ show (e::IOException)
      promptUniq m xs

module Library where
-- reusable function used in more than one place
import Control.Monad (forM)
import System.Random (randomRIO)
import Data.Array.IO (writeArray, IOArray, readArray, newListArray)
import System.IO (hFlush, stdout)

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

-- Prompt for a string, with a message
promptS :: String -> IO String
promptS m = putStr m >> hFlush stdout >> getLine

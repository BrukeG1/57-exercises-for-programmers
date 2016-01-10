module P37PasswordGen where

import Control.Monad
import Control.Exception
import Data.Array.IO
import System.Random

-- TODO: add pwd to clipboard; vowel replacement

main :: IO ()
main = do
    x <- promptN "How many passwords would you like suggested?: " 1
    l <- promptN "Password length: " 8
    s <- promptN "Number of numbers: " 1
    n <- promptN "Number of symbols: " 1
    putStrLn $ "Here are " ++ show x ++ " suggested passwords: "
    forM_ [1..x] $ \_ -> do
      p <- mkPass l s n
      putStrLn p

mkPass :: Int -> Int -> Int -> IO String
mkPass l s n = do
    ss <- get s syms
    ns <- get n nums
    cs <- get c chrs
    shuffle (ss ++ ns ++ cs)
  where
    c = l - s - n

get :: Int -> [a] -> IO [a]
get n = liftM (take n) . shuffle

-- Randomly shuffle a list
-- /O(N)/ from: https://wiki.haskell.org/Random_shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
   ar <- nArray n xs
   forM [1..n] $ \i -> do
     j <- randomRIO (i,n)
     vi <- readArray ar i
     vj <- readArray ar j
     writeArray ar j vi
     return vj
  where
    n = length xs
    nArray :: Int -> [a] -> IO (IOArray Int a)
    nArray n' =  newListArray (1,n')

chrs,nums,syms::String
chrs  = "abcdefghijklmnopqrstuvwxyz"
nums  = "01233456789"
syms  = "!$%^&*(){}[]:;@'~#<>,.?/"

promptN :: String -> Int -> IO Int
promptN m mn = do
    putStr m
    x <- readLn `catch` except
    if x<mn
      then do
        putStrLn $ "Numbers must be greater than or equal to " ++ show mn
        promptN m mn
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptN m mn

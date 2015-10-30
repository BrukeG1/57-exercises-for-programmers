import System.Random
import Data.Array.IO (writeArray, IOArray, readArray, newListArray)
import Control.Monad
 
main :: IO ()
main = do
    n <- prompt "Noun: "
    v <- prompt "Verb: "
    adj <- prompt "Adjective: "
    adv <- prompt "Adverb: "
    mkStory v n adj adv >>= putStrLn

mkStory :: String -> String -> String -> String -> IO String
mkStory v n adj adv =
    liftM head $ shuffle (stories v n adj adv)

prompt :: String -> IO String
prompt s = do
    putStr s
    getLine

stories :: String -> String -> String -> String -> [String]
stories v n adj adv = 
    [ "Do you " ++ v ++ " your " ++ adj ++ " " ++ n ++ " " ++ adv ++ "? That's hilarious!"
    , "Oh flip! Why do you " ++ v ++ " this " ++ adj ++ " " ++ n ++ "? Weirdo."
    , "How does " ++ v ++ " a " ++ adj ++ " " ++ n ++ " work?"
    ]

-- Randomly shuffle a list from https://wiki.haskell.org/Random_shuffle
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


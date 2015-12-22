{-# LANGUAGE ScopedTypeVariables #-}
import Text.Printf (printf)
import Control.Exception (catch, IOException)

tip, total :: Float -> Float -> Float
tip amt rate = amt * (rate / 100)
total amt rate = amt + tip amt rate

promptF :: String -> IO Float
promptF s = do
    putStr s
    (readLn :: IO Float) `catch` \ (ex :: IOException) -> promptFErr ex s

promptFErr :: IOException -> String -> IO Float
promptFErr ex s = do
    putStr "Couldn't parse that as a number. Haskell says:\n  "
    print ex
    promptF s

promptC :: String -> IO Char
promptC s = do
    putStr s
    getChar

continue :: IO ()
continue = do
    c <- promptC "Continue (Y/n) ? "
    putStrLn ""
    if c == 'N' || c == 'n'
      then putStrLn "Bye!"
      else main

main :: IO ()
main = do
    amt <- promptF "Bill amount: "
    rate <- promptF "Tip rate as percentage: "
    if rate > 0 && amt > 0
      then do
        putStrLn $ printf "Tip: $%.2f" (tip amt rate)
        putStrLn $ printf "Rate: $%.2f" (total amt rate)
        continue
      else do
        putStrLn "Invalid input: must be both positive numbers"
        continue

module P35Winner where

import System.Random
import Control.Monad (unless)

-- TODO: GUI, contest registration programme

main :: IO ()
main = do
  xs <- promptXs "Enter a name (empty for last): " []
  oneRound xs

oneRound :: [String] -> IO ()
oneRound xs = do
  i <- randomRIO(0,length xs - 1)
  putStrLn $ "And the winner is: " ++ show (xs !! i)
  putStr "Choose another winner? (empty for no, anything else for yes): "
  again <- getLine
  unless (again == "") $
    if length xs == 1
      then putStrLn "We're out of people to choose winners from."
      else oneRound $ deleteAt i xs

deleteAt :: Int -> [a] -> [a]
deleteAt i xs =
    l ++ r
  where 
    (l, _:r) = splitAt i xs

promptXs :: String -> [String] -> IO [String]
promptXs m xs = do
    putStr m
    x <- getLine
    if x == ""
      then return xs
      else do ys <- promptXs m xs
              return $ x:ys

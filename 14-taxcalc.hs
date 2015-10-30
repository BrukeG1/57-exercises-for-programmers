import Text.Printf (printf)
import Data.Char (toUpper)
import Control.Monad (when) -- to meet constraint of having no "else" clause but note that
                            -- without mutation you have to faff some to make it work

main :: IO ()
main = do
    amt       <- promptN "Amount: "
    state     <- promptS "State:  "
    putStrLn $ "Total (exc tax): " ++ showD amt
    when (uc state == "WI") $ wisconsinTax amt

showD :: Float -> String
showD = printf "%8.2f"

uc :: String -> String
uc = map toUpper

wisconsinTax :: Float -> IO ()
wisconsinTax a = do
    putStrLn $ "Tax:             " ++ showD tax
    putStrLn $ "Total:           " ++ showD (a + tax)
  where
    tax = a * 0.055

promptS :: String -> IO String
promptS m = do
    putStr m
    getLine

promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    x <- readLn -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x
-- TODO: Full state name lookup

module P17BloodAlcohol where

import Library

data Gender = Male | Female

main :: IO ()
main = do
    g <- promptG "Gender: "
    a <- promptNonNegNum "Amount of alcohol in Oz:  "
    w <- promptNonNegNum "Weight in lb: "
    h <- promptNonNegNum "Hours since last drink: "
    let b = bac a w h g
    putStrLn $ "Your blood alcohol content is: "
                ++ show b
    putStrLn $ "It is " ++  (if isLegal b then "" else  "not ") ++ "legal for you to drive"

r :: Gender -> Double
r Male = 0.73
r Female = 0.66

bac :: Double -> Double -> Double -> Gender -> Double
bac a w h g = let b =  (a * 5.14 / w * r g) - 0.15 * h
              in if b<0 then 0 else b

isLegal :: Double -> Bool
isLegal b = b < 0.08

promptG :: String -> IO Gender
promptG m = do
    putStr m
    c <- getLine
    case c of
      "M" -> return Male
      "F" -> return Female
      _ -> do putStrLn "Only M or F work for the formula"
              promptG m

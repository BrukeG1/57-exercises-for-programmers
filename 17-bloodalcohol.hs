data Gender = Male | Female

main :: IO ()
main = do
    g <- promptG "Gender: "
    a <- promptN "Amount of alcohol in Oz:  "
    w <- promptN "Weight in lb: "
    h <- promptN "Hours since last drink: "
    let b = bac a w h g
    putStrLn $ "Your blood alcohol content is: " 
                ++ show b
    putStrLn $ "It is " ++  if isLegal b then "" else  "not " ++ "legal for you to drive"

r :: Gender -> Double
r Male = 0.73
r Female = 0.66

bac :: Double -> Double -> Double -> Gender -> Double
bac a w h g = (a * 5.14 / w * r g) - 0.15 * h 

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

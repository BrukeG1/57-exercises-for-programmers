main :: IO ()
main = do
    l <- promptI "length in feet: "
    w <- promptI "width in feet: "
    let f2 = w * l
    putStrLn $ "Dimensions: " ++ show l ++  " feet by " ++ show w ++ " feet"
    putStrLn $ "Area: " ++ show f2 ++  " square feet"
    putStrLn $ "      " ++ show (f2tom2 f2) ++  " square meters"

f2tom2 :: Float -> Float
f2tom2 = (*) conversionFactor 
  where 
    conversionFactor = 0.09290304

promptI :: String -> IO Float
promptI s = do
    putStr s
    x <- readLn :: IO Float -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptI s
      else
        return x

-- TODO: allow data in meters too, GUI


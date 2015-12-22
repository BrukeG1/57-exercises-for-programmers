main :: IO ()
main = do
    principal <- promptN "Principal: "
    rate      <- promptN "Rate of interest: "
    years     <- promptN "Num years: "
    let interest = computeInterest principal rate years
        total = principal + interest
    putStrLn $ "After " ++ show years ++ " years at "
               ++ show rate ++ "%, the investment will be worth £"
               ++ show total ++ ", a growth of £" ++ show interest

computeInterest :: Float -> Float -> Float -> Float
computeInterest p r y = 
    p * (r/100) * y

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

-- TODO: Show amount at year end for each year

module P12SimpleInterest where

import Library

main :: IO ()
main = do
    principal <- promptNonNegFloat "Principal: "
    rate      <- promptNonNegFloat "Rate of interest: "
    years     <- promptNonNegFloat "Num years: "
    let interest = computeInterest principal rate years
        total = principal + interest
    putStrLn $ "After " ++ show years ++ " years at "
               ++ show rate ++ "%, the investment will be worth £"
               ++ show total ++ ", a growth of £" ++ show interest

computeInterest :: Float -> Float -> Float -> Float
computeInterest p r y = 
    p * (r/100) * y

-- TODO: Show amount at year end for each year

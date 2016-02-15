module P13CompoundInterest where

import Text.Printf (printf)
import Library

main :: IO ()
main = do
    principal <- promptNonNegFloat "Principal: "
    rate      <- promptNonNegFloat "Rate of interest: "
    years     <- promptNonNegFloat "Num years: "
    numComp   <- promptNonNegFloat "Num times compounded/year: "
    let interest = computeCompoundInterest principal rate years numComp
        total    = principal + interest
    putStrLn $ "After " ++ show years ++ " years at "
               ++ showD rate ++ "%, the investment will be worth £"
               ++ showD total ++ ", a growth of £" ++ showD interest

showD :: Float -> String
showD = printf "%0.2f"

computeCompoundInterest :: Float -> Float -> Float -> Float -> Float
computeCompoundInterest p r y n =
    read . showD $ p * (1+(r/100.0)/n) ^ (round (n*y)::Int) - p

-- TODO: reverse program i.e. given a total and a rate show the required principal
-- Make a GUI for it.

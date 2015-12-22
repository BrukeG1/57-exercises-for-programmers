import Text.Printf (printf)

main :: IO ()
main = do
    principal <- promptN "Principal: "
    rate      <- promptN "Rate of interest: "
    years     <- promptN "Num years: "
    numComp   <- promptN "Num times compounded/year: "
    let interest = computeCompoundInterest principal rate years numComp
        total    = principal + interest
    putStrLn $ "After " ++ show years ++ " years at "
               ++ showD rate ++ "%, the investment will be worth £"
               ++ showD total ++ ", a growth of £" ++ showD interest

showD :: Float -> String
showD = printf "%0.2f"

computeCompoundInterest :: Float -> Float -> Float -> Float -> Float
computeCompoundInterest p r y n = 
    p * (1+(r/100.0)/n) ^ (round (n*y)::Int) - p

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

-- TODO: reverse program i.e. given a total and a rate show the required principal
-- Make a GUI for it.

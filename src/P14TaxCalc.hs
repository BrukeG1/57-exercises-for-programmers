module P14TaxCalc where

import Control.Monad (when) -- to meet constraint of having no "else" clause but note that
                            -- without mutation you have to faff some to make it work
import Library

main :: IO ()
main = do
    amt       <- promptNonNegFloat "Amount: "
    state     <- promptS "State:  "
    putStrLn $ "Total (exc tax): " ++ showD amt
    when (uc state == "WI") $ putStrLn $ wisconsinTax amt

wisconsinTax :: Float -> String
wisconsinTax a =
       "Tax:             " ++ showD8 tax
    ++ "\n"
    ++ "Total:           " ++ showD8 (a + tax)
  where
    tax = a * 0.055

-- TODO: Full state name lookup

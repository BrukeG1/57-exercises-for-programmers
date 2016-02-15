module P05Maths where

import Library

main :: IO ()
main = do
    in1 <- promptNonNegFloat "Number 1: "
    in2 <- promptNonNegFloat "Number 2: "
    putStrLn $ calcPrint (show in1) (show in2)

calcPrint :: String -> String -> String
calcPrint in1 in2 =
       in1 ++ "+" ++ in2 ++ "=" ++ plus n1 n2 ++ "\n"
    ++ in1 ++ "-" ++ in2 ++ "=" ++ minus n1 n2 ++ "\n"
    ++ in1 ++ "/" ++ in2 ++ "=" ++ divBy n1 n2 ++ "\n"
    ++ in1 ++ "*" ++ in2 ++ "=" ++ times n1 n2 ++ "\n"
  where
    n1 = read in1::Float
    n2 = read in2::Float

plus, minus, divBy, times :: Float -> Float -> String
plus  n1 n2 = show $ n1+n2
minus n1 n2 = show $ n1-n2
divBy n1 n2 = show $ n1/n2
times n1 n2 = show $ n1*n2

-- todo: GUI

module P05Maths where

import Library

main :: IO ()
main = do
    in1 <- promptNonNegFloat "Number 1: "
    in2 <- promptNonNegFloat "Number 2: "
    putStrLn $ calcPrint (show in1) (show in2)

calcPrint :: String -> String -> String
calcPrint in1 in2 =
       in1 ++ "+" ++ in2 ++ "=" ++ show (n1 `plus` n2) ++ "\n"
    ++ in1 ++ "-" ++ in2 ++ "=" ++ show (n1 `minus` n2) ++ "\n"
    ++ in1 ++ "/" ++ in2 ++ "=" ++ show (n1 `divBy` n2) ++ "\n"
    ++ in1 ++ "*" ++ in2 ++ "=" ++ show (n1 `times` n2) ++ "\n"
  where
    n1 = read in1::Float
    n2 = read in2::Float

plus, minus, divBy, times :: Float -> Float -> Float
plus  = (+)
minus = (-)
divBy = (/)
times = (*)

-- todo: GUI

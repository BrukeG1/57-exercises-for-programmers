import Control.Monad

main :: IO ()
main = do
    in1 <- prompt "Number 1: "
    in2 <- prompt "Number 2: "
    putStrLn $ calcPrint in1 in2

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

prompt :: String -> IO String
prompt s = do
    putStr s
    x <- readLn :: IO Float -- could catch error here if we wanted to recover
    if x<0 
      then do
        putStrLn "Numbers must be non-negative"
        prompt s
      else
        return $ show x

-- todo: GUI

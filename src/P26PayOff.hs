module P26PayOff where
import Control.Exception (catch, IOException)
import Control.Monad (when)

-- TODO: work out the amount per month given the number of months

main :: IO ()
main = do
    b <- promptN "Balance on card: "
    apr <- promptN "APR (as a percentage): "
    p <- promptN "How much do you want to pay each month: "
    let n = monthsToPayOff apr b p
    when (n > 0) . putStrLn $ "It will take you " ++ show n ++ " months to pay it off"

monthsToPayOff :: Double -> Double -> Double -> Int
monthsToPayOff apr b p =
    if p < m
      then error $ "Your payment of " ++ show p
                 ++ " is less than the minimum payment of "
                 ++ show m
                 ++ " (if you are ever going to be able pay it off!)"
      else ceiling n
  where
    n = (-1/30) * log ( 1 + (b / p) * (1 - (1 + i) ** 30) )
                  / log (1 + i)
    i = apr / 36500
    m = b * (apr/1200)

promptN :: String -> IO Double
promptN m = do
    putStr m
    x <- readLn `catch` except
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x
  where
    except e = do
      putStrLn $ "Couldn't parse number. Error was: " ++ show (e::IOException)
      promptN m

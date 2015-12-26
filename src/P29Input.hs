module P29Input where
import Control.Exception (catch, IOException)

main :: IO ()
main = do
    putStrLn "Expected return calculator\n==========================\n"
    r <- promptN "What is the rate of return: "
    putStrLn $ "It will take about "
                ++ show (round $ expectedReturnOn r :: Int)
                ++ " years to double your initial investment"

expectedReturnOn :: Double -> Double
expectedReturnOn r = 72/r

promptN :: String -> IO Double
promptN msg = do
    putStr msg
    i <- readLn `catch` except
    case i of
      0 -> do
            putStrLn "Can't use 0 as input"
            promptN msg
      n -> return n
  where
    except :: IOException -> IO Double
    except _ = do
            putStrLn "Bad input, can't parse that as a number"
            promptN msg

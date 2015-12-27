module P31KarvonenHR where
import Control.Exception (catch, IOException)

-- todo: GUI

main :: IO ()
main = do
    age <- promptN "Age: "
    rhr <- promptN "Resting HR: "
    putStrLn "Intensity | Rate"
    putStrLn "----------------"
    putStrLn $ concatMap (showStep age rhr) [55,60..95]

showStep :: Double -> Double -> Double -> String
showStep age rhr intensity = 
    show intensity ++ "      | " ++ show thr ++ " bpm\n"
  where
    thr = targetHR age rhr intensity

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

targetHR :: Double -> Double -> Double -> Int
targetHR age rhr intensity =
    round (((220 - age - rhr) * i) + rhr)
  where
    i = intensity/100

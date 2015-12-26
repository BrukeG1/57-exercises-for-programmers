module P28Sum where
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    putStr "Number of inputs: "
    numReps <- readLn
    is <- promptForN numReps
    putStrLn $ "The total is: " ++ show (sum' is)

promptForN :: Int -> IO [Int]
promptForN 0 = return []
promptForN n = do
    putStr "Next number: "
    mi <- getLine
    let i = fromMaybe 0 (readMaybe mi)
    is <- promptForN (n-1) -- should liftIO or something here?
    return $ i : is

readMaybe :: Read a => String -> Maybe a
readMaybe s = 
  case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

module P33Magic8Ball where
import System.Random (randomRIO)

main :: IO ()
main = do
    putStrLn "Magic 8 Ball\n============\n"
    putStr "Enter your question: "
    _ <- getLine
    r <- randomRIO(0,3)
    putStrLn $ showMsg r

showMsg :: Int -> String
showMsg i | i == 0 = "Yes"
          | i == 1 = "No"
          | i == 2 = "Maybe"
          | i == 3 = "Ask again later"
          | otherwise = error "Problem input " ++ show i ++ " in showMsg"

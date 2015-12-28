module P32GuessNum where
import System.Random
import Control.Exception (catch, IOException)

main :: IO ()
main = do
    putStrLn "Guess Number\n==========\n"
    lvl <- promptLvl
    answer <- randomRIO(1,lvl)
    guessNum [] answer

guessNum :: [Int] -> Int -> IO ()
guessNum guesses answer = do
    guess <- promptN "Your guess: "
    if guess == answer
      then
        putStrLn . winMsg $ length guesses
      else do
        putStrLn (hint guess answer)
        if  guess `elem` guesses
          then do putStrLn $ "You already guessed " ++ show guess
                  guessNum (guess:guesses) answer
          else guessNum (guess:guesses) answer

hint :: Int -> Int -> String
hint g a | g < a     = "Too low!"
         | otherwise = "Too high!"

winMsg :: Int -> String
winMsg l | l == 0    = "You got it in 1 guess! You're a mindreader."
         | l <= 2    = "You got it in " ++ show l ++ ". Most impressive."
         | l <= 5    = "You got it in " ++ show l ++ ". You can do better than that."
         | otherwise = "You got it in " ++ show l ++ ". Better luck next time."

promptLvl :: IO Int
promptLvl = do
    putStr "What level (1/easy .. 3/hard): "
    x <- readLn `catch` except
    if x<1 && x>3
      then do
        putStrLn "Levels are 1 to 3"
        promptLvl
      else
        return $ case x of
          2 -> 100
          3 -> 1000
          _ -> 10
  where
    except e = do
      putStrLn $ "Couldn't parse level number. Error was: " ++ show (e::IOException)
      promptLvl

promptN :: String -> IO Int
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

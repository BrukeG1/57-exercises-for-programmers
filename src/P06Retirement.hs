import Data.Time
import System.Locale

data RetireWhen = Now Int | Future Int | Past Int

main :: IO ()
main = do
    year <- getYear
    age <- promptI "How old are you now? "
    ret <- promptI "Retirement age? "
    case retireWhen age ret year of
      Past p   -> putStrLn $ "You can already retire, " ++ show p
                             ++ " was in the past"
      Now n    -> putStrLn $ "You can already retire, " ++ show n
                             ++ " is this year. Speak to your adviser."
      Future f -> putStrLn $ "You can retire in " ++ show (f - year)
                             ++ " years, which is in the year " ++ show f

getYear :: IO Int
getYear = do
    now <- getCurrentTime
    return $ read (formatTime defaultTimeLocale "%Y" now)

retireWhen :: Int -> Int -> Int -> RetireWhen
retireWhen a r y =
    case compare (y + retirementDelta) y of
      LT -> Past   retirementYear
      EQ -> Now    retirementYear
      GT -> Future retirementYear
  where
    retirementDelta = r - a
    retirementYear = y + retirementDelta

promptI :: String -> IO Int
promptI s = do
    putStr s
    x <- readLn :: IO Int -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptI s
      else
        return x

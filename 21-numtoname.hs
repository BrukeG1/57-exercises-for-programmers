import Data.Map (fromList, (!), Map)

main :: IO ()
main = do
  putStrLn "Month lookup"
  m <- promptM "Enter month number (1..12): " 
  putStrLn $ getMonth m
  putStrLn $ getMonth' m

getMonth :: Int -> String
getMonth m =
  case m of
    1 -> "January"
    2 -> "February"
    3 -> "March"
    4 -> "April"
    5 -> "May"
    6 -> "June"
    7 -> "July"
    8 -> "August"
    9 -> "September"
    10 -> "October"
    11 -> "November"
    12 -> "December"
    _ -> error "Unknown month"

monthHash :: Map Int String
monthHash =
    fromList
      [ (1,  "January")
      , (2,  "February")
      , (3,  "March")
      , (4,  "April")
      , (5,  "May")
      , (6,  "June")
      , (7,  "July")
      , (8,  "August")
      , (9,  "September")
      , (10, "October")
      , (11, "November")
      , (12, "December")
      ]

getMonth' :: Int -> String
getMonth' = (monthHash !)

promptM :: String -> IO Int
promptM m = do
    putStr m
    x <- readLn -- could catch error here if we wanted to recover
    if x<1 || x>12
      then do
        putStrLn "Numbers must be between 1 and 12"
        promptM m
      else
        return x

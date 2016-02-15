module P01Hello where

main :: IO ()
main = do
  putStr "What is your name? "
  name <- getLine
  putStrLn $ chooseGreet name

chooseGreet :: String -> String
chooseGreet name =
  case name of
    "Charlie"       -> "Greetings, professor. How about a nice game of chess?"
    "David Cameron" -> "PIGFUCKER!"
    _               -> "Hello, " ++ name ++ ", how do you do?"


main :: IO ()
main = do
  putStr "What is your name? "
  name <- getLine
  case name of 
    "Charlie"       -> putStrLn "Greetings, professor Charlie. How about a nice game of chess?"
    "David Cameron" -> putStrLn "PIGFUCKER!"
    _               -> putStrLn $ "Hello, " ++ name ++ ", how do you do?"

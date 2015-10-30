main :: IO ()
main = do
    putStr "Input string: "
    s <- getLine
    if null s 
      then do 
        putStrLn "Must enter /something/"
        main
      else do 
        let n = length s
        putStrLn $ "'" ++ s ++ "' has " ++ show n ++ " characters"

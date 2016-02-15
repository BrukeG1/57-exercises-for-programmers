module P02NumChars where

main :: IO ()
main = do
    putStr "Input string: "
    s <- getLine
    case validate s of
      Nothing -> putStrLn "Must enter /something/" >> main
      Just is -> putStrLn is

validate :: String -> Maybe String
validate is | null is   = Nothing
            | otherwise = Just $ "'"
                                 ++ is
                                 ++ "' has "
                                 ++ show (length is)
                                 ++ " character"
                                 ++ if length is > 1 then "s" else ""

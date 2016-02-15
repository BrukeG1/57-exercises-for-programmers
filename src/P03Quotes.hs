module P03Quotes where

main :: IO ()
main = do
  q <- prompt "Quote: "
  w <- prompt "Who: "
  putStrLn $ mkQuote w q

mkQuote :: String -> String -> String
mkQuote w q = w ++ " says, \"" ++ q ++ "\""

prompt :: String -> IO String
prompt s = do
    putStr s
    getLine

-- TODO: build a list of these

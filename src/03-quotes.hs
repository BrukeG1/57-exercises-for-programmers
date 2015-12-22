main :: IO ()
main = do
  q <- prompt "Quote: "
  w <- prompt "Who: "
  putStrLn $ w ++ " says, \"" ++ q ++ "\""

prompt s = do
    putStr s
    getLine

-- TODO: build a list of these

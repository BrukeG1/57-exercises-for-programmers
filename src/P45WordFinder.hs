module P45WordFinder where
import Data.List (intercalate)
import Data.List.Split (splitOn)

inFile :: FilePath
inFile = "wordFinderInput.txt"

outFile :: FilePath
outFile = "wordFinderOutput.txt"

main :: IO ()
main = do
    txt <- readFile inFile
    let (ws, count) = replace "utilize" "use" txt
    _ <- writeFile outFile ws
    putStrLn $ (show count) ++ " occurrences of 'utilize' replaced."

replace :: String -> String -> String -> (String,Int)
replace word replacement ws =
    (ws', length splits - 1)
  where
    splits = splitOn word ws  -- make a list using the word to replace as delimeter
    ws'    = intercalate replacement splits -- shove the list back together using
                                            -- the replacement word as the delimeter

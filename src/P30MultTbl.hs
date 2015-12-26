module P30MultTbl where
import Text.Printf(printf)

-- todo: gui

main :: IO ()
main = putStrLn . table $ multTbl 12

numTab :: String
numTab = "%3d "

format :: Int -> String
format  = printf numTab

table :: [(Int,Int,Int)] -> String
table xs =
     spaces ++ header ++ tbl
  where
    spaces  = "    "
    header  = concatMap format [0 .. largest]
    largest = maximum xss
    xss     = map frst xs
    frst (a,_,_)
            = a
    tbl     = mkTbl (-1) xs

mkTbl :: Int -> [(Int,Int,Int)] -> String
mkTbl _ [] = []
mkTbl cur ((a,_,c):xs) =
    if a == cur
      then format c ++ mkTbl cur xs
      else "\n" ++ format (cur + 1) ++ format c ++ mkTbl (cur + 1) xs

multTbl :: Int -> [(Int,Int,Int)]
multTbl n =
    if n < 1
      then [(0,0,0)]
      else [(x,y,x*y) | x <- [0..n], y <- [0..n]]


main :: IO () 
main = tinsForLRoom


tinsForLRoom :: IO ()
tinsForLRoom = do
    w <- promptF "How wide is your room? "
    l <- promptF "And how long is it? "
    il <- promptF "What is the width of the sideways L-bit ? "
    iw <- promptF "And the width of the vertical L-bit? "
    let top = tinsOfPaint $ area w l iw il
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for an L-shaped room of width " 
                ++ show w ++ ", length " ++ show l
                ++ ", inside width " ++ show iw
                ++ " and inside length " ++ show il
                ++ " with total area " ++ show (area w l iw il)

  where
    area w l iw il = w*l - (w-iw)*(l-il)
    


tinsForRoundRoom :: IO ()
tinsForRoundRoom = do
    d <- promptF "How wide is your room? "
    let top = tinsOfPaint $ area d
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for a circular room of diameter " 
                ++ show d ++ " and area " ++ show (area d)
  where
    area d = round (((3.142*(radius d))^2)::Float) 
    radius d = fromIntegral d/2 

tinsForRectRoom :: IO ()
tinsForRectRoom = do
    w <- promptF "How wide is your room? "
    l <- promptF "And how long is it? "
    let top = tinsOfPaint $ area l w
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for a rectangular room of " 
                ++ show w ++ "x" ++ show l ++ " and area " ++ show (area l w)
  where
    area l w = l * w

coverage :: Int
coverage = 350

tinsOfPaint :: Int -> Int
tinsOfPaint area =
    if remArea > 0
      then tins + 1
      else tins
  where
    (tins, remArea) = area `divMod` coverage


promptF :: String -> IO Int
promptF m = do
    putStr m
    x <- readLn :: IO Int -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptF m
      else
        return x

--- TODO: Make a mobile app?!

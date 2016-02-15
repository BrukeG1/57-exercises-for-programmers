module P09Paint where

import Library

main :: IO () 
main = tinsForLRoom

-- how much coverage in square units we get from a tin of paint
coverage :: Int
coverage = 350

tinsForRoundRoom :: IO ()
tinsForRoundRoom = do
    d <- promptNonNegInt "How wide is your room? "
    let top = tinsOfPaint $ circArea d
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for a circular room of diameter " 
                ++ show d ++ " and area " ++ show (circArea d)

tinsForRectRoom :: IO ()
tinsForRectRoom = do
    w <- promptNonNegInt "How wide is your room? "
    l <- promptNonNegInt "And how long is it? "
    let top = tinsOfPaint $ rectArea l w
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for a rectangular room of " 
                ++ show w ++ "x" ++ show l ++ " and area " ++ show (rectArea l w)

tinsForLRoom :: IO ()
tinsForLRoom = do
    w <- promptNonNegInt "How wide is your room? "
    l <- promptNonNegInt "And how long is it? "
    il <- promptNonNegInt "What is the width of the inner L-bit ? "
    iw <- promptNonNegInt "And the length of the inner L-bit? "
    let top = tinsOfPaint $ elArea w l iw il
    putStrLn $ "You will need " ++ show top 
                ++ " tins of paint for an L-shaped room of width " 
                ++ show w ++ ", length " ++ show l
                ++ ", inside width " ++ show iw
                ++ " and inside length " ++ show il
                ++ " with total area " ++ show (elArea w l iw il)

circArea :: Int -> Int
circArea d =
    round (pi*(radius d ** 2)::Float)

radius :: Int -> Float
radius d = fromIntegral d/2

rectArea :: Int -> Int -> Int
rectArea l w = l * w

elArea :: Int -> Int -> Int -> Int -> Int
elArea w l iw il = w*l - iw*il

tinsOfPaint :: Int -> Int
tinsOfPaint area =
    if remArea > 0
      then tins + 1
      else tins
  where
    (tins, remArea) = area `divMod` coverage

--- TODO: Make a mobile app?!

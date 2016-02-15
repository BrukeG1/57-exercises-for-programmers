module P07RoomArea where

import Library

main :: IO ()
main = do
    l <- promptNonNegFloat "length in feet: "
    w <- promptNonNegFloat "width in feet: "
    let f2 = f2ed w  l
    putStrLn $ "Dimensions: " ++ show l ++  " feet by " ++ show w ++ " feet"
    putStrLn $ "Area: " ++ show f2 ++  " square feet"
    putStrLn $ "      " ++ show (f2tom2 f2) ++  " square meters"

f2ed :: Float -> Float -> Float
f2ed w l = w * l

f2tom2 :: Float -> Float
f2tom2 =
    (*) conversionFactor
  where
    conversionFactor = 0.09290304

-- TODO: allow data in meters too, GUI

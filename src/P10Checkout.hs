module P10Checkout where

import Library
import Control.Monad
import Text.Printf(printf)

main :: IO ()
main = do
  n <- promptNonNegInt "How many items: "
  items <- mapM getItem [1..n]
  mapM_ showItem items
  subtotal <- foldM totalize 0 items
  tax <- getTax subtotal
  putStrLn $ "Subtotal: " ++ show subtotal
  putStrLn $ "Tax: " ++ show tax
  putStrLn $ "Total: " ++ show (subtotal + tax)


totalize :: Float -> (Int, Float, Int) -> IO Float
totalize s (_,price,qty) =
    return . read $ printf "%0.2f" tot
  where
    tot = s + price * fromIntegral qty

getTax :: Float -> IO Float
getTax subtotal =
    return $ subtotal * 0.055

showItem :: (Int, Float, Int) -> IO ()
showItem (i,price,qty) =
  putStrLn $ "Item " ++ show i 
            ++ " qty: " ++ show qty 
            ++ " price: " ++ show price

getItem :: Int -> IO (Int, Float, Int)
getItem i = do
    putStrLn $ "Item " ++ show i
    price <- promptNonNegFloat "Price: "
    qty <- promptNonNegInt "Qty: "
    return (i,price,qty)

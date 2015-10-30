import Control.Monad

main :: IO ()
main = do
  n <- promptN "How many items: "
  items <- mapM getItem [1..n]
  mapM_ showItem items
  subtotal <- foldM totalize 0 items
  tax <- getTax subtotal
  putStrLn $ "Subtotal: " ++ show subtotal
  putStrLn $ "Tax: " ++ show tax
  putStrLn $ "Total: " ++ show (subtotal + tax)


totalize :: Float -> (Int, Float, Int) -> IO Float
totalize s (_,price,qty)=
    return $ s + price * fromIntegral qty

getTax :: Float -> IO Float
getTax subtotal = return $ subtotal * 0.055

showItem :: (Int, Float, Int) -> IO ()
showItem (i,price,qty) =
  putStrLn $ "Item " ++ show i 
            ++ " qty: " ++ show qty 
            ++ " price: " ++ show price

getItem :: Int -> IO (Int, Float, Int)
getItem i = do
    putStrLn $ "Item " ++ show i
    price <- promptN "Price: "
    qty <- promptN "Qty: "
    return (i,price,qty)

promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    x <- readLn -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptN m
      else
        return x

module P38Filter where

filt :: (a -> Bool) -> [a] -> [a]
filt _ [] = []
filt p (x:xs) = if p x then x : filt p xs else filt p xs

evn :: Integer -> Bool
evn n = n `mod` 2 == 0

od :: Integer -> Bool
od = not . evn

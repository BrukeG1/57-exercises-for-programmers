import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "Temp converter options"
    putStrLn ""
    u <- promptS "What units is your temperature in (C,F,K): "
    t <- promptN "What is the temperature?: "
    to   <- promptS "What unit do you want to convert to (C,F,K): "
    let temp = case u of
                    "C" -> Celsius t
                    "F" -> Farenheit t
                    "K" -> Kelvin t
                    _   -> error "Bad source unit"
    let result = case to of
                    "C" -> toCelsius temp
                    "F" -> toFarenheit temp
                    "K" -> toKelvin temp
                    _   -> error "Bad target unit"
    putStrLn $ show temp ++ " converts to " ++ show result

promptS :: String -> IO String
promptS m = do
    putStr m
    s <- getLine
    case map toUpper s of 
      "K" -> return "K"
      "C" -> return "C"
      "F" -> return "F"
      _   -> do putStrLn "Must be one of C,F,K"
                promptS m

promptN :: (Num a, Ord a, Read a) => String -> IO a
promptN m = do
    putStr m
    readLn -- could catch error here if we wanted to recover

data Temperature = Celsius Float | Farenheit Float | Kelvin Float

instance Eq Temperature where
    Celsius x == Celsius y = x == y
    x == y                 = toCelsius x == toCelsius y

instance Show Temperature where
    show (Celsius x)   = show x ++ "°C"
    show (Farenheit x) = show x ++ "°F"
    show (Kelvin x)    = show x ++ "K"

instance Num Temperature where
    (Celsius x) + (Celsius y) = Celsius (x + y)
    x + y                     = toCelsius x + toCelsius y
    (Celsius x) - (Celsius y) = Celsius (x - y)
    x - y                     = toCelsius x - toCelsius y
    (Celsius x) * (Celsius y) = Celsius (x * y)
    x * y                     = toCelsius x * toCelsius y
    abs (Celsius x)           = Celsius (abs x)
    abs x                     = abs (toCelsius x)
    fromInteger i             = Celsius (fromInteger i)
    signum (Celsius x)        = Celsius (signum x)     -- doesn't make much sense though!
    signum x                  = signum (toCelsius x)

toCelsius :: Temperature -> Temperature
toCelsius (Kelvin k)    = Celsius $ k-273.15
toCelsius (Farenheit f) = Celsius $ (f-32)*5/9
toCelsius           c   = c

toFarenheit :: Temperature -> Temperature
toFarenheit t =
    Farenheit $ 32+(c*9/5)
  where
    (Celsius c) = toCelsius t

toKelvin :: Temperature -> Temperature
toKelvin t =
    Kelvin $ c + 273.15
  where
    (Celsius c) = toCelsius t

-- TODO: make a GUI

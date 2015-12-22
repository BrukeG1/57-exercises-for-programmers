{-# LANGUAGE DeriveGeneric #-}
import Text.Printf (printf)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)

data Conversion =
  Conversion { success :: Bool
             , target :: !Text
             , rate :: Float
             , source :: !Text
             , amount :: Float
             , message :: !Text
               } deriving (Show, Generic)
instance FromJSON Conversion
instance ToJSON Conversion

main :: IO () 
main = do
    euros <- promptF "Euros: "
    rateFrom <- promptF "Exchg rate: "
    let dollars = getDollars euros rateFrom
    putStrLn $ "Using fixed rates, that is $" ++ dollars 
               ++ ", making the dollar rate (rate to) "
               ++ show (rateTo dollars euros)
    mr <- apiConvert euros "EUR" "USD"
    case mr of
      Nothing -> putStrLn "There was an error reading the JSON data."
      Just r  -> putStrLn $ "Using API rate, €" ++ show euros 
                            ++ " is equivalent to $" 
                            ++ show r ++ ", making the dollar rate (to): "
                            ++ show (rateTo dollars euros)
                            ++ " and the euro rate (from) "
                            ++ show (rateTo (show euros) (read dollars))



getDollars :: Float -> Float -> String
getDollars e r = printf "%.2f" (e * (r/100))

rateTo :: String -> Float -> Float
rateTo d e = e / read d * 100

promptF :: String -> IO Float
promptF m = do
    putStr m
    x <- readLn :: IO Float -- could catch error here if we wanted to recover
    if x<0
      then do
        putStrLn "Numbers must be non-negative"
        promptF m
      else
        return x

-- this needs running via stack to get the right version of the libraries
getConversion :: String -> String -> Float -> IO (Maybe Conversion)
getConversion from to q =
  fmap decode $ simpleHttp $
      "https://currency-api.appspot.com/api/" 
      ++ from ++ "/" ++ to ++ ".json?amount=" ++ show q

apiConvert :: Float -- ^ Initial quantity.
        -> String -- ^ Initial currency.
        -> String -- ^ Target currency.
        -> IO (Maybe Float) -- ^ Result.
apiConvert q from to = fmap (fmap amount) $ getConversion from to q

-- TODO: Read conversion rate from an API 

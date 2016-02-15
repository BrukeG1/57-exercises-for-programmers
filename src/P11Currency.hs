{-# LANGUAGE DeriveGeneric #-}

module P11Currency where

import Text.Printf (printf)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import Control.Applicative((<$>))
import Library

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
    euros <- promptNonNegFloat "Euros: "
    rateFrom <- promptNonNegFloat "Exchg rate: "
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
                            ++ show (rateTo (show r) euros)
                            ++ " and the euro rate (from) "
                            ++ show (rateTo (show euros) r)

getDollars :: Float -> Float -> String
getDollars e r = printf "%.2f" (e * (r/100))

rateTo :: String -> Float -> Float
rateTo d e = e / read d * 100

getConversion :: String -> String -> Float -> IO (Maybe Conversion)
getConversion frm tO q =
  fmap decode $ simpleHttp $
      "https://currency-api.appspot.com/api/" 
      ++ frm ++ "/" ++ tO ++ ".json?amount=" ++ show q

apiConvert :: Float -- ^ Initial quantity.
        -> String -- ^ Initial currency.
        -> String -- ^ Target currency.
        -> IO (Maybe Float) -- ^ Result.
apiConvert q frm tO = fmap amount <$> getConversion frm tO q

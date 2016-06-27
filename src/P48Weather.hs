{-# LANGUAGE OverloadedStrings #-}
module P48Weather where

import Control.Applicative (Const)
import Control.Lens ((^.),(^?))
import Data.Aeson.Lens (_String, _Number, _Integer, key, nth, AsValue)
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (First)
import Data.Scientific (toRealFloat, Scientific)
import Data.Text (unpack, Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale,formatTime)
import Library (promptS)
import Network.Wreq (get, Response, responseBody)
import PrivateConfig (weatherAPIKey)

-- Might as well use our code from earlier for temp conversions (though
-- the API can do this for us if required)
import P18TempConverter (Temperature (Kelvin), toCelsius, toFarenheit)

data CompassDirection = North
                      | NorthNorthEast
                      | NorthEast
                      | EastNorthEast
                      | East
                      | EastSouthEast
                      | SouthEast
                      | SouthSouthEast
                      | South
                      | SouthSouthWest
                      | SouthWest
                      | WestSouthWest
                      | West
                      | WestNorthWest
                      | NorthWest
                      | NorthNorthWest
                      deriving (Show, Eq)

main :: IO ()
main = do
    p <- promptS "Where are you?: "
    w <- weatherForPlace p
    putStrLn $ "Current temperature is "
             ++ (show . toFarenheit $ getTemp w)
             ++ " ("
             ++ (show . toCelsius $ getTemp w)
             ++ ")"
    putStrLn $ "Weather description: " ++ unpack (getDescription w)
    putStrLn $ "Wind direction is " ++ show (getWindDirection w)
    putStrLn $ "Wind speed is " ++ show (getWindSpeed w) ++ " meter/sec"
    putStrLn $ "Air pressure is " ++ show (getPressure w) ++ " hPa"
    putStrLn $ "Sun rises at " ++ getSunrise w
    putStrLn $ "Sun sets at " ++ getSunset w
    putStrLn $ "Humidity is " ++ show (getHumidity w) ++ "%"

weatherForPlace :: String -> IO (Response ByteString)
weatherForPlace place = get $ "http://api.openweathermap.org/data/2.5/weather?q="
                              ++ place
                              ++ "&appid="
                              ++ weatherAPIKey

-- Given a Response, a lens focus, a conversion function and the item name we're extracting
-- return the item in some useful format or throw an error (with the unretrievable item name)
getItem :: Response body0
           -> ((a -> Const (First a) a) -> body0 -> Const (First a) body0)
           -> (a -> t)
           -> String
           -> t
getItem w l cf itm =
    case t of
      (Just n) -> cf n
      Nothing  -> error $ "Could not retrieve " ++ itm
  where
    t = w ^? responseBody . l

getTemp :: AsValue b => Response b -> Temperature
getTemp w =
    getItem w (key "main" . key "temp" . _Number) (Kelvin . toRealFloat) "temperature"

getHumidity :: AsValue b => Response b -> Integer
getHumidity w =
    getItem w (key "main" . key "humidity" . _Number) round "humidity"

getPressure :: AsValue b => Response b -> Integer
getPressure w =
    getItem w (key "main" . key "pressure" . _Number) round "pressure"

getWindSpeed :: AsValue b => Response b -> Double
getWindSpeed w =
    getItem w (key "wind" . key "speed" . _Number) toRealFloat "wind speed"

getWindDirection :: AsValue b => Response b -> CompassDirection
getWindDirection w =
    getItem w (key "wind" . key "deg" . _Number) (degToDirection . toRealFloat) "wind direction"

degToDirection :: Float -> CompassDirection
degToDirection d | (d >= 348.75 && d <= 360)
                   || d >= 0 && d <=  11.25 = North
                 | d >   11.25 && d <   33.75 = NorthNorthEast
                 | d >=  33.75 && d <=  56.25 = NorthEast
                 | d >   56.25 && d <   78.75 = EastNorthEast
                 | d >=  78.75 && d <= 101.25 = East
                 | d >  101.25 && d <  123.75 = EastSouthEast
                 | d >= 123.75 && d <= 146.25 = SouthEast
                 | d >  146.25 && d <  168.75 = SouthSouthEast
                 | d >= 168.75 && d <= 191.25 = South
                 | d >  191.25 && d <  213.75 = SouthSouthWest
                 | d >= 213.75 && d <= 236.25 = SouthWest
                 | d >  236.25 && d <  258.75 = WestSouthWest
                 | d >= 258.75 && d <= 281.25 = West
                 | d >  281.25 && d <  303.75 = WestNorthWest
                 | d >= 303.75 && d <= 326.25 = NorthWest
                 | d >  326.25 && d <  348.75 = NorthNorthWest
                 | otherwise            = error $ "Bad wind direction: " ++ show d

getDescription :: AsValue b => Response b -> Text
getDescription w =
    getItem w (key "weather" . nth 0 . key "description" . _String) id "description"

getSunrise :: AsValue b => Response b -> String
getSunrise w =
    getItem w (key "sys" . key "sunrise" . _Integer) unixToDateString "sunrise"

getSunset :: AsValue b => Response b -> String
getSunset w =
    getItem w (key "sys" . key "sunset" . _Integer) unixToDateString "sunset"

unixToDateString =
    formatTime defaultTimeLocale  "%Y-%m-%d %H:%M:%S %Z" . posixSecondsToUTCTime . fromInteger

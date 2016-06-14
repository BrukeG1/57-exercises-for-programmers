module P50Movie where
import Library
import PrivateConfig (rotTomAPIKey) -- for API key

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
import Network.Wreq (get, Response, responseBody)

main :: IO ()
main = undefined

-- title, year, rating, running time, description, watch or not

getMovie :: String -> IO (Response ByteString)
getMovie title = get $ "http://api.rottentomatoes.com/api/public/v1.0/movies.json?apikey="
                          ++ rotTomAPIKey
                          ++ "&q="
                          ++ title
                          ++ "&page_limit=1&page=1"

--getTitle :: AsValue b => Response b -> Temperature
--getTitle w = -- getItem w (key "main" . key "temp" . _Number) (Kelvin . toRealFloat) "temperature"

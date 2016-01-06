{-# LANGUAGE DeriveGeneric #-}
import Text.Printf (printf)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (maximumBy, sortBy)
import Data.Function (on)

data Person =
  Person { name :: Text
         , craft :: Text
         } deriving (Show, Generic)
instance FromJSON Person 
instance ToJSON Person

data InSpace =
  InSpace { people :: [Person]
          , number :: Int
          , message :: !Text
          } deriving (Show, Generic)
instance FromJSON InSpace 
instance ToJSON InSpace

getInSpace :: IO (Maybe InSpace)
getInSpace = do
  fmap decode $ simpleHttp $ "http://api.open-notify.org/astros.json"

format :: InSpace -> String
format is =
    header ++ concatMap row peeps
  where
    numPeeps        = show $ number is
    peeps           = sortBy (compare `on` (last . words . T.unpack . name)) $ people is
    header          = "There are " ++ numPeeps ++ " people in space at the moment\n"
                      ++ printf nameColFmt "Name"
                      ++ printf craftColFmt "Craft"
                      ++ "\n--------------------------------\n"
    row p           = nameFmt p ++ craftFmt p
    nameFmt  p      = printf nameColFmt (T.unpack $ name p)
    craftFmt p      = printf craftColFmt (T.unpack $ craft p)
    nameColFmt      = "%" ++ (show $ longestBy name peeps) ++ "s | "
    craftColFmt     = "%" ++ (show $ longestBy craft peeps) ++ "s \n"
    longestBy f     = (lengthBy f) . maximumBy (compare `on` (lengthBy f))
    lengthBy f      = T.length . f

main :: IO ()
main = do
    i <- getInSpace
    case i of
        Just is -> putStrLn $ format is
        Nothing -> error "API problem"

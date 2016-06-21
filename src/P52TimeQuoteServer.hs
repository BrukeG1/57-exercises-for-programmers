{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module P52TimeQuoteServer where

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Time
import Data.Time.Format (formatTime)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs
import Servant.Client
import qualified Network.HTTP.Client as C

import Library

-- API defined in the type
type TimeQuoteAPI = "time"  :> Get '[JSON] Time
               :<|> "quote" :> Get '[JSON] Quote

data Time = Time { currentTime  :: UTCTime
                 } deriving (Show, Eq, Generic)

instance ToJSON Time

data Quote = Quote { quote :: String
                   , source :: String
                   } deriving (Show, Eq, Generic)

instance ToJSON Quote

-- Server
main :: IO ()
main = run 8081 app

app :: Application
app = serve timeQuoteAPI server

timeQuoteAPI :: Proxy TimeQuoteAPI
timeQuoteAPI = Proxy

server :: Server TimeQuoteAPI
server = liftIO getTime
    :<|> liftIO getQuote

getTime :: IO Time
getTime = do
    t <- getCurrentTime
    return $ Time t

getQuote :: IO Quote
getQuote = do
    qs <- shuffle quotes
    return $ head qs

-- Some quotes
quotes = [ Quote "The three chief virtues of a programmer are: Laziness, Impatience and Hubris." "Larry Wall"
         , Quote "The bearing of a child takes nine months, no matter how many women are assigned." "Fred Brooks"
         , Quote "Show me your flowcharts and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won't usually need your flowcharts; they'll be obvious." "Fred Brooks"
         , Quote "The art of programming is the art of organizing complexity, of mastering multitude and avoiding its bastard chaos as effectively as possible." "Edsger Dijkstra"
         , Quote "I'm doing a free operating system (just a hobby, won't be big and professional like gnu) for 386(486) AT clones." "Linus Torvalds"
         , Quote "You've baked a really lovely cake, but then you've used dog shit for frosting." "Steve Jobs"
         , Quote "What is wanted here is something like the following substitution property: If for each object o1 of type S there is an object o2 of type T such that for all programs P defined in terms of T, the behavior of P is unchanged when o1 is substituted for o2 then S is a subtype of T." "Barbara Liskov"
         , Quote "If it's a good idea, go ahead and do it. It's much easier to apologize than it is to get permission. " "Grace Hopper"
         , Quote "A designer knows he has achieved perfection not when there is nothing left to add, but when there is nothing left to take away." "Antoine de Saint-Exupery"
         ]

-- Documentation
instance ToSample Quote where
  toSamples _ =
    singleSample
      (Quote "A designer knows he has achieved perfection not when there is nothing left to add, but when there is nothing left to take away." "Antoine de Saint-Exupery")

instance ToSample Time where
  toSamples _ =
    singleSample
      (Time (read "1970-01-01 00:00:00"::UTCTime))

apiDocs :: API
apiDocs = docs timeQuoteAPI

documentation :: String
documentation = markdown apiDocs

-- Client
instance FromJSON Quote
instance FromJSON Time

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081 ""

manager :: IO C.Manager
manager = C.newManager C.defaultManagerSettings

clientTime :: C.Manager -> BaseUrl -> ExceptT ServantError IO Time
clientQuote :: C.Manager -> BaseUrl -> ExceptT ServantError IO Quote
clientTime :<|> clientQuote = client timeQuoteAPI

timeQuoteQueries :: C.Manager -> ExceptT ServantError IO (Time, Quote)
timeQuoteQueries manager = do
  t <- clientTime manager baseUrl
  q <- clientQuote manager baseUrl
  return (t,q)

runclient :: IO ()
runclient = do
  m <- manager
  res <- runExceptT (timeQuoteQueries m)
  case res of
    Left err -> putStrLn $ "ERR: " ++ show err
    Right (t,q) -> do
      putStrLn $ "The current time is " ++ prettyTime t
      putStrLn $ "\nA quote:\n" ++ quote q ++ "\n\t\t\t\t\t\t- " ++ source q

prettyTime :: Time -> String
prettyTime t =
    formatTime defaultTimeLocale "%H:%M:%S on %A, %d %B, %Y" h
  where
    h = currentTime t

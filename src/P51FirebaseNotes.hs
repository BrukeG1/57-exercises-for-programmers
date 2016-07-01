{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module P51FirebaseNotes where

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Time
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Format (formatTime)
import GHC.Generics
import System.Environment (getArgs)
import Servant
-- import Servant.Docs
import Servant.Client
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Client.TLS as CT

import Library

data Note =
    Note { ctime :: UTCTime
         , text :: String
         } deriving (Eq, Generic)

instance ToJSON Note

instance FromJSON Note

-- Usual order is most recent note first
instance Ord Note where
    compare (Note t1 _ ) (Note t2 _) = t2 `compare` t1

-- TODO: Do we really need picoseconds in time?
instance Show Note where
    show (Note ctime text) = show ctime ++ ": " ++ show text

main :: IO ()
main = fclient

fclient :: IO ()
fclient = do
    argv <- getArgs
    if null argv
      then error usage
      else case head argv of
            "show" -> do ns <- getNotes
                         mapM_ print ns
            "post"  -> do n <- postNote . unwords $ tail argv
                          putStrLn n
            "help" -> putStrLn usage
            _      -> error $ "Unrecognized command\n" ++ usage

usage :: String
usage = "Usage: P51FirebaseNotes [help | show | post <xxx>]"

type NoteAPI = "note.json" :> Get '[JSON] (Map String Note)
          :<|> "note.json" :> ReqBody '[JSON] Note :> Post '[JSON] (Map String String) 

noteAPI :: Proxy NoteAPI
noteAPI = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "notes-b1434.firebaseio.com" 443 ""

manager :: IO C.Manager
manager = C.newManager CT.tlsManagerSettings

(getNs :<|> postN) = client noteAPI

-- to consider: just returning some number of notes
-- to discover: does firebase keep things in order or do I still need to do sorting here?
getNotes :: IO [Note]
getNotes = do
    m <- manager
    res <- runExceptT (getNs m baseUrl)
    case res of
      Left err -> error $ "ERR: " ++ show err
      Right ns -> return . sort $ M.elems ns

postNote :: String -> IO String
postNote t = do
    m <- manager
    c <- getCurrentTime
    let note = Note c t
    res <- runExceptT (postN note m baseUrl)
    case res of
      Left err -> error $ "ERR: " ++ show err
      Right n -> return $ "Inserted note with id: " ++ head (M.elems n)

--egclient :: IO ()
--egclient = do
--    n <- postNote "test API implementation"
--    print =<< getNotes

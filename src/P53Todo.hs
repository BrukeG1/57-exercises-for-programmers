{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module P53Todo where

import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time
import Data.Time.Format (formatTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Servant.Docs
import qualified Network.HTTP.Client as C

import Library

-- database config
db = "todos.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Todo json
   task     Text
   priority Int
   start    UTCTime
   due      UTCTime
   created  UTCTime default=CURRENT_TIME
   deriving Show
|]

type DbKey = BackendKey SqlBackend

-- API type
type TodoAPI =
         "todo" :> Get '[JSON] [Todo]
    :<|> "todo" :> ReqBody '[JSON] Todo   :> Put '[JSON] (Key Todo)
    :<|> "todo" :> Capture "id" DbKey     :> Get '[JSON] (Maybe Todo)
    :<|> "todo" :> ReqBody '[JSON] (DbKey, Todo)
                                          :> PostNoContent '[JSON] ()
    :<|> "todo" :> Capture "id" DbKey     :> DeleteNoContent '[JSON] ()

-- main: run our server
main :: IO ()
main = run 8081 app

-- API server implementation
todoServer :: Server TodoAPI
todoServer =
       liftIO listTodos
  :<|> liftIO . createTodo
  :<|> liftIO . retrieveTodo
  :<|> liftIO . updateTodo
  :<|> liftIO . deleteTodo

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

-- API documentation
instance ToSample Todo where
  toSamples _ =
    singleSample
      (Todo "Task"
            3
            (read "2016-04-04 14:44:44"::UTCTime)
            (read "2016-11-01 11:11:11"::UTCTime)
            (read "2016-11-01 11:11:11"::UTCTime)
      )

instance ToSample DbKey where
  toSamples _ =
    singleSample $ toBackendKey (toSqlKey 4::TodoId)

instance ToSample (Key Todo) where
  toSamples _ =
    singleSample (toSqlKey 4::TodoId)

instance ToCapture (Capture "id" DbKey) where
  toCapture _ =
    DocCapture "id"
               "Database id of the Todo"

apiDocs :: API
apiDocs = docs todoAPI

documentation :: String
documentation = markdown apiDocs

-- API commandline client
baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081 ""

manager :: IO C.Manager
manager = C.newManager C.defaultManagerSettings

clientList :: C.Manager -> BaseUrl -> ClientM [Todo]
clientCreate :: Todo -> C.Manager -> BaseUrl -> ClientM (Key Todo)
clientRetrieve :: DbKey -> C.Manager -> BaseUrl -> ClientM (Maybe Todo)
clientUpdate :: (DbKey,Todo) -> C.Manager -> BaseUrl -> ClientM ()
clientDelete :: DbKey -> C.Manager -> BaseUrl -> ClientM ()
clientList :<|> clientCreate
           :<|> clientRetrieve
           :<|> clientUpdate
           :<|> clientDelete = client todoAPI

clientEg :: IO ()
clientEg = do
  m <- manager
  res <- runExceptT (clientList m baseUrl) -- or clientRetrieve, etc
  case res of
    Left x   -> putStrLn $ "Error: " ++ show x
    Right ns -> mapM_ print ns

-- Database interactions (CRUD, list, migrate), also used by API implementation
createTodo :: Todo -> IO (Key Todo)
createTodo t =
    runSqlite db $ insert t

retrieveTodo :: DbKey -> IO (Maybe Todo)
retrieveTodo key =
    runSqlite db $ get (TodoKey key)

updateTodo :: (DbKey, Todo) -> IO ()
updateTodo (key, t) =
    runSqlite db $ update (TodoKey key) us
  where
    us = [TodoTask =. todoTask t]

deleteTodo :: DbKey -> IO ()
deleteTodo key =
    runSqlite db $ delete (TodoKey key)

listTodos :: IO [Todo]
listTodos = do
    es <- runSqlite db $ selectList [] []
    return $ map entityVal es

migrateTodo :: IO ()
migrateTodo =
    runSqlite db $ runMigration migrateTables

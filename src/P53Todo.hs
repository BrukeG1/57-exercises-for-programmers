{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
    :<|> "todo" :> Capture "id" DbKey     :> DeleteNoContent '[JSON] ()
    :<|> "todo" :> ReqBody '[JSON] (DbKey, Todo)
                                          :> PostNoContent '[JSON] ()

-- main: run our server
main :: IO ()
main = run 8081 app

-- API server implementation
-- CRUD functionality is in the database section below
todoServer :: Server TodoAPI
todoServer =
       liftIO listTodos
  :<|> liftIO . createTodo
  :<|> liftIO . retrieveTodo
  :<|> liftIO . deleteTodo
  :<|> liftIO . updateTodo

app :: Application
app = serve todoAPI todoServer

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

-- API documentation
--
--   task     Text
--   priority Int
--   start    UTCTime
--   due      UTCTime
--   created  UTCTime default=CURRENT_TIME
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
  toSamples _ = singleSample $ toBackendKey (toSqlKey 4::TodoId)

instance ToSample (Key Todo) where
  toSamples _ = singleSample (toSqlKey 4::TodoId)

instance ToCapture (Capture "id" DbKey) where
  toCapture _ =
    DocCapture "id"
               "Database id of the Todo"

apiDocs :: API
apiDocs = docs todoAPI

documentation :: String
documentation = markdown apiDocs

-- Database interactions (CRUD, list, migrate), also used by API implementation
createTodo :: Todo -> IO (Key Todo)
createTodo t = runSqlite db $ insert t

retrieveTodo :: DbKey -> IO (Maybe Todo)
retrieveTodo key =
    runSqlite db $ get (TodoKey key)

updateTodo :: (DbKey, Todo) -> IO ()
updateTodo (key, t) = do
    _ <- runSqlite db $ update (TodoKey key) us
    return ()
  where
    us = [TodoTask =. todoTask t]

deleteTodo :: DbKey -> IO ()
deleteTodo key = do
    _ <- runSqlite db $ delete (TodoKey key)
    return ()

listTodos :: IO [Todo]
listTodos = do
    es <- runSqlite db $ selectList [] []
    return $ map entityVal es

migrateTodo :: IO ()
migrateTodo =
    runSqlite db $ runMigration migrateTables

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators #-}

module P53Todo where

import Control.Monad.Trans
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
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

db = "todos.sqlite"

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Todo
   task     Text
   priority Int
   start    UTCTime
   due      UTCTime
   created  UTCTime default=CURRENT_TIME
   deriving Show
|]

main :: IO ()
main = do
    time <- liftIO getCurrentTime
    -- runSqlite db $ runMigration migrateTables
    -- key <- createTodo "Test task" 1 time time time
    -- print key
    td <-  retrieveTodo 1
    print td
    return ()

createTodo :: Text -> Int -> UTCTime -> UTCTime -> UTCTime -> IO (Key Todo)
createTodo t p s e c =
    runSqlite db $ insert $ Todo t p s e c

retrieveTodo :: BackendKey SqlBackend -> IO (Maybe Todo)
retrieveTodo key =
    runSqlite db $ get (TodoKey key)

-- updateTodo
-- deleteTodo
-- listTodos

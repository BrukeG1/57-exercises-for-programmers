{-# LANGUAGE OverloadedStrings #-}
module P52TimeQuoteServerSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Wai
import Network.Wai.Test (simpleBody)
import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy (toStrict)
import P52TimeQuoteServer hiding (main)

main :: IO ()
main = hspec spec

spec = with (return app) $ do
    it "root responds with 404" $
      get "/" `shouldRespondWith` 404
    it "non-existent responds with 404" $
      get "/thisdoesnotexist" `shouldRespondWith` 404
    it "quote responds with 200" $
      get "/quote" `shouldRespondWith` 200
    it "time responds with 200" $
      get "/time" `shouldRespondWith` 200
    it "time responds with correct content-type for /time" $
      get "/time" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "application/json" ] }
    it "time responds with correct content-type for /quote" $
      get "/quote" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "application/json" ] }
    it "/time contains {\"currentTime\": in body" $ do
      r <- get "/time"
      liftIO $ toStrict (simpleBody r)  `shouldSatisfy` ("{\"currentTime\":" `isInfixOf`)
    it "/quote contains {\"quote\": in body" $ do
      r <- get "/quote"
      liftIO $ toStrict (simpleBody r)  `shouldSatisfy` ("{\"quote\":" `isInfixOf`)
    it "/quote contains \"source\": in body" $ do
      r <- get "/quote"
      liftIO $ toStrict (simpleBody r)  `shouldSatisfy` ("\"source\":" `isInfixOf`)

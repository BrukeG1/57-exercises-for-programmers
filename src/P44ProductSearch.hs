{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module P44ProductSearch where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Library

-- TODO: add files back to JSON file if product not found

catalogueFile :: String
catalogueFile =  "productCatalogue.json"

data Product =
   Product { name :: String
           , price :: Float
           , quantity :: Int
           } deriving (Eq, Show, Generic)
instance FromJSON Product
instance ToJSON Product

data Catalogue =
    Catalogue { products :: [Product]
              } deriving (Eq, Show, Generic)
instance FromJSON Catalogue
instance ToJSON Catalogue

main :: IO ()
main = do
    p <- promptS "Product name: "
    putStrLn ""
    ps <- findByName p
    if null ps
      then do
        putStrLn "Not found"
        main
      else 
        mapM_ printProd ps


printProd p = do
    putStrLn $ "Name: " ++ (name p)
    putStrLn $ "Price: £" ++ (show $ price p)
    putStrLn $ "Quantity on hand: " ++ (show $ quantity p)
    putStrLn "--"

catalogue :: IO (Maybe Catalogue)
catalogue =
    fmap decode $ B.readFile catalogueFile

findByName :: String -> IO [Product]
findByName x = do
    c <- catalogue
    return $ case c of
      (Just cat) -> filter (\p -> lc x `isInfixOf` lc (name p)) $ products cat
      Nothing    -> []

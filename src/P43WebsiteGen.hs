{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module P43WebsiteGen where
import Text.Hamlet (shamletFile)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)
import System.Directory(createDirectory)
import Library
import Config

data Meta = Meta
    { title   :: String
    , author  :: String
    }

main :: IO ()
main = do
    sitename <- promptS "Site name: "
    author   <- promptS "Author: "
    js       <- promptS "Javascript folder? : "
    css      <- promptS "CSS folder? : "
    let sitePath = websiteGenBasePath ++ (filter (/='.') sitename)
        indexhtml = indexHTML $ Meta sitename author
    createDirectory sitePath
    mkdirIf sitePath "js" js
    mkdirIf sitePath "css" css
    writeFile (sitePath ++ "/index.html") indexhtml

mkdirIf :: Filepath -> Filepath -> String -> IO ()
mkdirIf sitePath path ok =
  if ok!!0 == 'y' || ok!!0 == 'Y'
    then createDirectory $ sitePath ++ "/" ++ path
    else return ()

indexHTML :: Meta -> String
indexHTML meta =
    renderHtml $(shamletFile $ websiteGenSkeletonPath ++ "index.hamlet")

module Config where

type Filepath = String

websiteGenBasePath :: Filepath
websiteGenBasePath = "websitegen/"

websiteGenSkeletonPath :: Filepath
websiteGenSkeletonPath = websiteGenBasePath ++ ".skeleton/"

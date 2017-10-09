{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config      (readConfig)
import           Export      (sendEmail)
import           Paths_crawl (getDataFileName)
import           Reddit      (checkLengthAndSkip, getPosts)

{-|
1. Read config/config.yml
2. Crawl ML Subreddit
3. Send email when it's done
-}
main :: IO ()
main = do
  filename <- getDataFileName "config/config.yml"
  configData <- readConfig filename

  case configData of
    Just (user, pass, recipients) -> do
      posts <- getPosts >>= checkLengthAndSkip

      case posts of
        Just posts -> sendEmail user pass recipients (posts)
        Nothing    -> print "Failed to fetch any post"

    Nothing -> do
      print "Failed to read config.yml"

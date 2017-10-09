{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Reddit
import Export (sendEmail)
import Config (readConfig)
import Paths_crawl (getDataFileName)

main :: IO ()
main = do
  filename <- getDataFileName "config/config.yml"
  configData <- readConfig filename
  case configData of
    Just (user, pass, recipients) -> do
      posts <- getPosts
      case posts of
        Just posts ->
          sendEmail user pass recipients posts
        Nothing ->
          print "Failed to fetch any post"
    Nothing -> do
      print "Failed to read config.yml"

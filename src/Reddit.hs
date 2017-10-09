{-# LANGUAGE OverloadedStrings #-}
module Reddit where

import Text.HTML.Scalpel
import Network.Curl.Opts

type Title = String
type Link = String

data RedditPost = RedditPost Title Link

instance Show RedditPost where
  show (RedditPost title link) = title ++ "\n" ++ link

redditURL :: String
redditURL = "https://www.reddit.com/r/MachineLearning/"

curlOptions :: [CurlOption]
curlOptions =
  [ CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36"
  , CurlReferer "Referer:https://www.reddit.com/r/MachineLearning/"
  ]

getPosts :: IO (Maybe [RedditPost])
getPosts = scrapeURLWithOpts curlOptions redditURL posts
   where
       posts :: Scraper String [RedditPost]
       posts = chroots ("div" @: [hasClass "thing"]) post

       post :: Scraper String RedditPost
       post = do
         title <- text $ "a" @: [hasClass "title"]
         link <- attr "href" $ "a" @: [hasClass "bylink"]
         return $ RedditPost title link


{-|
First two rediit posts are pinned posts
So, skip the two items
-}
skipFirstTwoItems :: [RedditPost] -> [RedditPost]
skipFirstTwoItems = tail . tail

checkLengthAndSkip :: Maybe [RedditPost] -> IO (Maybe [RedditPost])
checkLengthAndSkip Nothing = return Nothing
checkLengthAndSkip (Just posts) =
  if length posts > 2
  then return (Just $ skipFirstTwoItems posts)
  else return Nothing

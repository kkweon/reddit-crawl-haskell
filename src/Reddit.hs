{-# LANGUAGE OverloadedStrings #-}
module Reddit where

import           Data.List         (isInfixOf)
import           Network.Curl.Opts
import           Text.HTML.Scalpel

{-|
RedditPost Data Type
-}
type Title = String
type Link = String

data RedditPost = RedditPost Title Link

{-|
Shows the reddit post in the following format
"title\nlink"
-}
instance Show RedditPost where
  show (RedditPost title link) = title ++ "\n" ++ link

{-|
Scrap this page
-}
redditURL :: String
redditURL = "https://www.reddit.com/r/MachineLearning/"

{-|
Without these options, Reddit does not allow to crawl
-}
curlOptions :: [CurlOption]
curlOptions =
  [ CurlUserAgent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36"
  , CurlReferer "Referer:https://www.reddit.com/r/MachineLearning/"
  ]

{-|
Scalpel does all the magic here
-}
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

-- | If title contains the following string, skip
skipList :: [String]
skipList = ["Machine Learning - WAYR (What Are You Reading)"]

noGoodPost :: Maybe [RedditPost] -> IO (Maybe [RedditPost])
noGoodPost Nothing = return Nothing
noGoodPost (Just posts) =
  return . Just $ filterUnless isInSkipList posts
  where
    filterUnless f = filter (not . f)
    isInSkipList (RedditPost title _) = foldr (\x acc -> (isInfixOf x title) || acc) False skipList

{-# LANGUAGE OverloadedStrings #-}
module Reddit where

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


{-|
First two rediit posts are pinned posts
which are not useful, so skip the first two items
-}
skipFirstTwoItems :: [RedditPost] -> [RedditPost]
skipFirstTwoItems = tail . tail

{-|
If there are more than two, skip two
If not, something wrongs (Nothing)
-}
checkLengthAndSkip :: Maybe [RedditPost] -> IO (Maybe [RedditPost])
checkLengthAndSkip Nothing = return Nothing
checkLengthAndSkip (Just posts) =
  if length posts > 2
  then return (Just $ skipFirstTwoItems posts)
  else return Nothing

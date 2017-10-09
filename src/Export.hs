{-# LANGUAGE OverloadedStrings #-}

module Export where

import Data.Text (pack)
import qualified Data.Text.Lazy as T
import Network.Mail.Client.Gmail (sendGmail)
import Network.Mail.Mime (Address(Address))
import Reddit (RedditPost(RedditPost))
import System.Environment

prepareEmailBody :: [RedditPost] -> String
prepareEmailBody = unlines . zipWith (\a b -> show a ++ ". " ++ show b) [1 ..]

oneSecond :: Int
oneSecond = 10 ^ 6


type Username = String
type Password = String

sendEmail :: Username -> Password -> [Address] -> [RedditPost] -> IO ()
sendEmail username password recipients posts =
  let sender = pack $ username ++ "@gmail.com"
  in sendGmail
       (T.pack username)
       (T.pack password)
       (Address (Just "Reddit Subscriptions") sender)
       recipients
       []
       []
       "Reddit Posts"
       (T.pack $ prepareEmailBody posts)
       []
       (oneSecond * 60)

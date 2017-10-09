{-# LANGUAGE OverloadedStrings #-}

module Export where

import           Data.Text                 (pack)
import qualified Data.Text.Lazy            as T
import           Network.Mail.Client.Gmail (sendGmail)
import           Network.Mail.Mime         (Address (Address))
import           Reddit                    (RedditPost (RedditPost))

{-|
Prepare email body

Example:

  ghci> prepareEmailBody posts :: String
  "1. [D] RedditPost  \n
  http://redditlink   \n
  2. ...              \n
  ...                 \n
  "

-}
prepareEmailBody :: [RedditPost] -> String
prepareEmailBody = unlines . zipWith (\a b -> show a ++ ". " ++ show b) [1 ..]

{-|
10^6 microsecond is one second
`sendGmail` has a parameter timeout (in microsecond)
-}
oneSecond :: Int
oneSecond = 10 ^ 6


type Username = String
type Password = String

{-|
Sends email with all these options

Example:

  ghci> sendEmail "kkweon" "password" [Address (Just "Mo") "kkweon@gmail.com"] posts
-}
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

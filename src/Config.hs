{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import Data.Text as T
import GHC.Generics
import Network.Mail.Mime (Address(Address))

data ConfigData = ConfigData
  { username :: String
  , password :: String
  , recipients :: [Recipient]
  } deriving (Show, Generic)

data Recipient = Recipient
  { email :: String
  , name :: String
  } deriving (Show, Generic)

instance FromJSON ConfigData

instance FromJSON Recipient

recipientToAddress :: Recipient -> Address
recipientToAddress (Recipient e n) =
  let
    name = Just (T.pack n)
    email = T.pack e
  in
    Address name email

readConfig :: String -> IO (Maybe (String, String, [Address]))
readConfig filename = do
  content <- BS.readFile filename -- (4)
  let parsedContent = Y.decode content :: Maybe ConfigData -- (5)
  case parsedContent of
    Nothing -> return $ Nothing
    Just (ConfigData u p rs) ->
      let addressList = Prelude.map recipientToAddress rs
      in return $ Just (u, p, addressList)

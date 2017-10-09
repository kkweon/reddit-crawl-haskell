{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.Text             as T
import qualified Data.Yaml             as Y
import           GHC.Generics
import           Network.Mail.Mime     (Address (Address))

{-|
Format `config/config.yml` should match this
-}
data ConfigData = ConfigData
  { username   :: String
  , password   :: String
  , recipients :: [Recipient]
  } deriving (Show, Generic)

{-|
Recipient Format inside of `recipients:`
-}
data Recipient = Recipient
  { email :: String
  , name  :: String
  } deriving (Show, Generic)

{-|
Necessary to use `Y.decode`
-}
instance FromJSON ConfigData
instance FromJSON Recipient

{-|
`sendEmail` takes data type `Address`
Hence, cast `Recipient` to `Address`
-}
recipientToAddress :: Recipient -> Address
recipientToAddress (Recipient e n) =
  let name = Just (T.pack n)
      email = T.pack e
  in Address name email

{-|
Reads `filename` and parse ConfigData
-}
readConfig :: String -> IO (Maybe (String, String, [Address]))
readConfig filename = do
  content <- BS.readFile filename -- (4)
  let parsedContent = Y.decode content :: Maybe ConfigData -- (5)

  case parsedContent of

    Nothing ->
      return $ Nothing

    Just (ConfigData u p rs) ->
      let addressList = Prelude.map recipientToAddress rs
      in return $ Just (u, p, addressList)

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import           Data.List
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

$(deriveJSON defaultOptions ''Day)

data User = User {
    firstName :: String
  , lastName  :: String
  , ssn       :: SSN
  , email     :: Email
  , address   :: Address
  , phone     :: String
  , dob       :: Day
  } deriving (Show, Generic)

data Address = Address {
    street    :: String
  , city      :: String
  , state     :: String
  , aptNumber :: String
  , zipCode   :: String
  } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

-- instance FromJSON Day where
--   parseJSON (String d) = readTime defaultTimeLocale "%Y-%m-%d" d
--   parseJSON _ = fail "failed to parse date"

-- instance ToJSON Day where
--   -- display a day in YYYY-mm-dd format
--   toJSON d = toJSON $ showGregorian d

instance ToJSON Email
instance FromJSON Email
instance ToJSON SSN
instance FromJSON SSN
instance ToJSON Address
instance FromJSON Address

newtype SSN = SSN String deriving (Show, Generic)
newtype Email = Email String deriving (Show, Generic)

-- type UserAPI =
--     -- GET /users/:id
--          "users" :> QueryParam "id" Integer :> Get '[JSON] [User]
--     -- :<|>
--     -- POST /users
-- userAPI :: Proxy UserAPI
-- userAPI = Proxy

-- -- server :: Server userAPI
-- server = undefined

-- app :: Application
-- app = serve userAPI server

-- runServer :: Port -> IO ()
-- runServer port = run port app

main :: IO ()
main = putStrLn "starting app on port 8081"
    -- runServer 8081

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

type UserAPI =
    -- GET /users
         "users" :> Get '[JSON] [User]
    -- GET /users/:id
    :<|> "users" :> Capture "id" Integer :> Get '[JSON] User
    -- POST /users
    :<|>"users" :> ReqBody '[JSON] User :> Post '[JSON] User
    -- DELETE /users/:id
    :<|>"users" :> Capture "id" Text :> Delete '[JSON] ()


theirDob = fromGregorian 2012 1 1
theirAddress = Address "Sttreet" "LA" "CA" "#1" "90210"

allUsers :: [User]
allUsers = [User "burt" "bobby" (SSN "123-23-5433") (Email "derp@gmail.com") theirAddress "218-222-5555" theirDob]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = allUsersH :<|> getUserH :<|> postUserH :<|> deleteUserH
  where
    allUsersH :: EitherT ServantErr IO [User]
    allUsersH = return allUsers

    getUserH :: Integer -> EitherT ServantErr IO User
    getUserH id = return $ Data.List.head allUsers

    postUserH :: User -> EitherT ServantErr IO User
    postUserH = return

    deleteUserH _ = return ()

app :: Application
app = serve userAPI server

runServer :: Port -> IO ()
runServer port = run port app

main :: IO ()
main = do
  putStrLn "starting app on port 8081"
  runServer 8081

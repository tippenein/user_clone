{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module UserService.Server
  ( userAPI
  , allUsers
  , runServer)
where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH              (defaultOptions, deriveJSON)
import Data.List
import Data.Text
import Data.Time
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

import UserService.Types

instance ToJSON Day where
  toJSON = toJSON . showGregorian

parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

instance ToJSON User
instance FromJSON User
instance FromJSON Day where
  parseJSON (Object v) = liftM parseDate (v .: "date")

instance ToJSON Email
instance FromJSON Email
instance ToJSON SSN
instance FromJSON SSN
instance ToJSON Address
instance FromJSON Address

type UserAPI =
  -- GET /users
  "users"
  :> QueryParam "id" Integer
  :> QueryParam "ssn" SSN
  :> QueryParam "email" Email
  :> Get '[JSON] [User]
    -- GET /users/:id
  :<|> "users" :> Capture "id" Integer :> Get '[JSON] User
  -- POST /users
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User
  -- DELETE /users/:id
  :<|> "users" :> Capture "id" Text :> Delete '[JSON] ()

allUsers :: [User]
allUsers = [User 1 "burt" "bobby" ssn email theirAddress "218-222-5555" theirDob]
  where
    ssn = SSN "123-23-5433"
    email = Email "derp@gmail.com"
    theirDob = fromGregorian 2012 1 1
    theirAddress = Address "Sttreet" "LA" "CA" "#1" "90210"

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = allUsersH :<|> getUserH :<|> postUserH :<|> deleteUserH
  where
    allUsersH :: Maybe Integer -> Maybe SSN -> Maybe Email -> EitherT ServantErr IO [User]
    allUsersH id ssn email = return allUsers

    getUserH :: Integer -> EitherT ServantErr IO User
    getUserH id = return $ Data.List.head allUsers

    postUserH :: User -> EitherT ServantErr IO User
    postUserH = return

    deleteUserH _ = return ()

app :: Application
app = serve userAPI server

runServer :: Port -> IO ()
runServer port = run port app


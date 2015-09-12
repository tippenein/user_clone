{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.List
import           Data.Time
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type UserAPI =
    -- GET /users/:id
         "users" :> QueryParam "id" Integer :> Get '[JSON] [User]
    -- :<|>
    -- POST /users

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

newtype SSN = SSN String deriving Show
newtype Email = Email String deriving Show

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server userAPI
server = undefined

app :: Application
app = serve userAPI server

runServer :: Port -> IO ()
runServer port = run port app

main :: IO ()
main = do
    putStrLn "starting app on port 8081"
    runServer 8081

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserService.Client
  ( getUsers
  , getUser
  , destroyUser
  , postUser)
where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Text
import GHC.Generics               (Generic)
import Servant
import Servant.Client

import UserService.Server
import UserService.Types

-- getUsers' :: Maybe Integer -> Maybe Email -> EitherT ServantError IO [User]

getUsers' :<|> getUser' :<|> postUser' :<|> destroyUser' =
  client userAPI host
    where
      host = BaseUrl Https "localhost" 8081

getUsers id email = runEitherT $ getUsers' (Just id) (Just email)
getUser id = runEitherT $ getUser' id
postUser user = runEitherT $ postUser' user
destroyUser id = runEitherT $ destroyUser' id

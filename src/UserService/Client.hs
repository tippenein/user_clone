{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserService.Client
  ( getUsers
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

-- getUsers' :: Maybe Integer -> Maybe SSN -> Maybe Email -> EitherT ServantError IO [User]

getUsers' :<|> getUser' :<|> postUser' :<|> destroyUser' =
  client userAPI host
    where
      host = BaseUrl Https "localhost" 8081

getUsers id ssn email = runEitherT $ getUsers' (Just id) (Just ssn) (Just email)

postUser user = undefined

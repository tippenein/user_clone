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

import UserService.API
import UserService.Server
import UserService.Types

-- getUsers' :: Maybe Integer -> Maybe Email -> EitherT ServantError IO [User]

type Action a = EitherT ServantError IO a

run :: Action a -> IO a
run action = do
    result <- runEitherT action
    case result of
        Left message -> error (show message)
        Right x -> return x

getUsers' :<|> getUser' :<|> postUser' :<|> putUser' :<|> destroyUser' =
  client userAPI host
    where
      host = BaseUrl Https "localhost" 8081

getUsers id email = runEitherT $ getUsers' (Just id) (Just email)
getUser id = runEitherT $ getUser' id
putUser id user = runEitherT $ putUser' id user
postUser user = runEitherT $ postUser' user
destroyUser id = runEitherT $ destroyUser' id

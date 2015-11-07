{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module UserService.API (userAPI, UserAPI) where

import Data.Proxy
import Data.Text
import Servant.API
import UserService.Types

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserAPI =
       ListUsers
  :<|> ShowUser
  :<|> CreateUser
  :<|> UpdateUser
  :<|> DestroyUser

type ListUsers = "users"
    :> QueryParam "id" Integer
    :> QueryParam "email" Text
    :> Get '[JSON] [User]

type ShowUser = "users"
    :> Capture "id" Integer
    :> Get '[JSON] User

type CreateUser = "users"
    :> ReqBody '[JSON] User
    :> Post '[JSON] User

type UpdateUser = "users"
    :> Capture "id" Integer
    :> ReqBody '[JSON] User
    :> Put '[JSON] User

type DestroyUser = "users"
    :> Capture "id" Integer
    :> Delete '[JSON] User


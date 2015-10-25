module UserService.Client
  ( getUsers
  , postUser)
where

import Control.Monad.Trans.Either
import Servant
import Servant.Client
import UserService.Server
import UserService.Types

(getUsers :<|> postUser) = client userAPI host
  where
    host = BaseUrl Https "localhost" 8081

    getUsers :: EitherT ServantError IO [User]
    getUsers = return allUsers

    postUser :: User -> EitherT ServantError IO User
    postUser u = return u

-- runQ :: IO ()
-- runQ = do
--   res <- runEitherT getUsers
--   case res of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right a -> print a


module UserService.Server
  (runServer)
where

import           Control.Monad.Trans.Either
import           Data.Text
import           Data.Time
import           Database.Persist
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           UserService.API            as API
import qualified UserService.Database       as DB
import           UserService.Types

type Handler a = EitherT ServantErr IO a

allUsers :: [User]
allUsers = [] :: [User]

server :: Server UserAPI
server =
       listUsers
  :<|> showUser
  :<|> createUser
  :<|> updateUser
  :<|> destroyUser

listUsers :: Maybe Integer
          -> Maybe Text
          -> Handler [User]
listUsers id email = return allUsers

showUser :: Integer -> Handler User
showUser id = return $ Prelude.head allUsers

createUser :: User -> Handler User
createUser u = return u

updateUser :: Integer -> User -> Handler User
updateUser id u = return u

destroyUser :: Integer -> Handler User
destroyUser id = return $ Prelude.head allUsers

app :: Application
app = serve API.userAPI server

runServer :: Port -> IO ()
runServer port = run port app


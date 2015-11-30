module UserService.Server
  (runServer)
where

import           Control.Monad.Trans.Either
import           Data.Text
import           Data.Time
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import           Control.Monad.IO.Class               (liftIO)
import           UserService.API                      as API
import qualified UserService.Database                 as DB
import           UserService.Types

type Handler a = EitherT ServantErr IO a

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
listUsers id email = return DB.allUsers

showUser :: Integer -> Handler User
showUser id = return $ Prelude.head DB.allUsers

createUser :: User -> Handler User
createUser = return

updateUser :: Integer -> User -> Handler User
updateUser id = return

destroyUser :: Integer -> Handler User
destroyUser id = return $ Prelude.head DB.allUsers

app :: Application
app = logStdout (serve API.userAPI server)

runServer :: Port -> IO ()
runServer port = run port app


{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module UserService.Database (setupDb) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Text
import           Data.Time
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Seeds                   as Seeds (users)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  firstName Text
  lastName Text
  email Text
  phone Text
  dob Day
  updatedAt UTCTime
  createdAt UTCTime
  deriving Show

Address
  user UserId
  street Text
  city Text
  state Text
  aptNumber Text
  zipCode Text
  updatedAt UTCTime
  createdAt UTCTime
  deriving Show
|]

allUsers = undefined

setupDb :: IO ()
setupDb = runSqlite ":memory:" $ do
  runMigration migrateAll
  now <- liftIO getCurrentTime
  _ <- insert $ User "derp" "town" "email@email.com" "123-223-3322" (fromGregorian 2000 2 2) now now
  liftIO $ print "migrated seeds"

-- $(makeAdaptorAndInstance "pUser" ''User)

-- usersTable :: Table ( Column PGText
--                     , Column PGText
--                     , Column PGText
--                     , Column PGText
--                     , Maybe Column PGText
--                     , Column PGText
--                     , Column PGDate)
-- usersTable = Table "usersTable" (p7 ( required "firstName"
--                                     , required "lastName"
--                                     , required "email"
--                                     , optional "address"
--                                     , required "phone"
--                                     , required "dob" ))


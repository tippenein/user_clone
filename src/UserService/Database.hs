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

module UserService.Database
  (setupDb, allUsers)
where

import           Data.Text
import           Data.Time
-- import           Database.PostgreSQL.Simple
-- import           Database.PostgreSQL.Simple.FromRow
-- import           Database.PostgreSQL.Simple.ToField
-- import           Database.PostgreSQL.Simple.ToRow
import qualified Seeds             as Seeds (users)
import           UserService.Types

-- instance FromRow Address where
--   fromRow = Address <$>
--       field <*>
--       field <*>
--       field <*>
--       field <*>
--       field

-- instance ToRow Address where
--   toRow Address{..} =
--     [ toField street
--     , toField city
--     , toField state
--     , toField aptNumber
--     , toField zipCode
--     ]

-- instance FromRow User where
--   fromRow = User <$>
--       field <*>
--       field <*>
--       field <*>
--       field <*>
--       field <*>
--       field

-- instance ToRow User where
--   toRow User{..} =
--     [ toField firstName
--     , toField lastName
--     , toField email
--     , toField address
--     , toField phone
--     , toField dob
--     ]

allUsers = Seeds.users
setupDb = undefined

-- setupDb = do
--   conn <- connect defaultConnectInfo { connectDatabase = "user_clone"}
--   execute conn "INSERT INTO users \
--     \ (first_name, last_name, email, address, phone, dob) \
--     \ VALUES (?,?,?,?,?,?)" Seeds.users
  -- putStrLn "setup database"

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


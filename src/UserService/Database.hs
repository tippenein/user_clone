{-# LANGUAGE TemplateHaskell #-}

module UserService.Database (usersTable) where

import Data.Profunctor.Product (p7)
import Opaleye
import UserService.Types

-- $(makeAdaptorAndInstance "pUser" ''User)

-- usersTable :: Table ( Column PGText
--                     , Column PGText
--                     , Column PGText
--                     , Column PGText
--                     , Maybe Column PGText
--                     , Column PGText
--                     , Column PGDate)
usersTable = Table "usersTable" (p7 ( required "firstName"
                                    , required "lastName"
                                    , required "ssn"
                                    , required "email"
                                    , optional "address"
                                    , required "phone"
                                    , required "dob" ))


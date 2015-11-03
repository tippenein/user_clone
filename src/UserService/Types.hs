{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UserService.Types (
    User(..)
  , Address(..)
  , Email(..)
  ) where

import Data.Text
import Data.Text.Internal
import Data.Time
import GHC.Generics
import Servant

data User = User {
    id        :: Integer
  , firstName :: Text
  , lastName  :: Text
  , email     :: Email
  , address   :: Address
  , phone     :: Text
  , dob       :: Day
  } deriving (Show, Generic)

data Address = Address {
    street    :: Text
  , city      :: Text
  , state     :: Text
  , aptNumber :: Text
  , zipCode   :: Text
  } deriving (Show, Generic)


newtype Email = Email Text deriving (Show, Generic, FromText, ToText)




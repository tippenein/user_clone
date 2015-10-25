{-# LANGUAGE DeriveGeneric #-}

module UserService.Types (
    User(..)
  , Address(..)
  , SSN(..)
  , Email(..)
  ) where

import Data.Text
import Data.Text.Internal
import Data.Time
import GHC.Generics

data User = User {
    firstName :: Text
  , lastName  :: Text
  , ssn       :: SSN
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


newtype SSN = SSN Text deriving (Show, Generic)
newtype Email = Email Text deriving (Show, Generic)




{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module UserService.Types (
    User(..)
  , Address(..)
  ) where

import Control.Monad
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Text
import Data.Text.Internal
import Data.Time
import GHC.Generics
import Servant

data User = User {
    id        :: Integer
  , firstName :: Text
  , lastName  :: Text
  , email     :: Text
  , address   :: Address
  , phone     :: Text
  , dob       :: Day
  } deriving (Show, Generic, FromJSON, ToJSON)

data Address = Address {
    street    :: Text
  , city      :: Text
  , state     :: Text
  , aptNumber :: Text
  , zipCode   :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)



parseDate :: String -> Day
parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"

instance FromJSON Day where
  parseJSON (Object v) = liftM parseDate (v .: "date")

instance ToJSON Day where
  toJSON = toJSON . showGregorian


{-# LANGUAGE OverloadedStrings #-}
module Seeds (users) where

import Data.Time
import UserService.Types

user1 = User "burt" "bobby" email theirAddress "218-222-5555" theirDob
  where
    email = "derp@gmail.com"
    theirDob = fromGregorian 2012 1 1
    theirAddress = Address "Sttreet" "LA" "CA" "#1" "90210"

user2 = User "basdf" "sdfawef" email theirAddress "218-222-5555" theirDob
  where
    email = "sdafasdf@gmail.com"
    theirDob = fromGregorian 2000 1 1
    theirAddress = Address "St ave goo" "Pelican Rapids" "MN" "#1" "56572"

users = [user1, user2]

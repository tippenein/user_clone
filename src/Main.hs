module Main where

import UserService

import Control.Monad
import System.Environment

import UserService.Database (setupDb)

-- | Main application init
main :: IO ()
main = do
  setupDb
  (port:_) <- getArgs
  putStrLn $ "starting app on port " ++ port
  runServer (fromIntegral (read port))


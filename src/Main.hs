module Main where

import UserService

import Control.Monad
import System.Environment

-- | Main application init
main :: IO ()
main = do
  (port:_) <- getArgs
  putStrLn $ "starting app on port " ++ port
  runServer (fromIntegral (read port))

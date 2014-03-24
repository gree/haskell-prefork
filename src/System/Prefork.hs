
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
  Module      : System.Prefork
  Copyright   : (c) 2013 Gree, Inc.
  License     : MIT-style
  
  Maintainer  : Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>
  Stability   : experimental
  Portability : portable

This is a library for servers based on worker process model (preforking).

Import System.Prefork in your Main module.

@
import System.Prefork
import System.Posix
import System.Exit (exitSuccess)
@

Define data type used for server configuration.

@
data ServerConfig = ServerConfig
@

Define workers as a data type.

@
data Worker = Worker1 String deriving (Show, Read)

instance WorkerContext Worker
@

Call System.Prefork.defaultMain function with settings in your main function.
 
@
main :: IO ()
main = defaultMain defaultSettings {
    psUpdateConfig = updateConfig
  , psUpdateServer = updateServer
  , psOnStart      = \_ -> do
      pid <- getProcessID
      putStrLn $ \"Please send SIGHUP to \" ++ show pid ++ \" to relaunch a worker\"
  } $ \so -> case so of
  Worker1 msg -> print msg >> exitSuccess

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (Worker1 \"Hello. I'm a worker.\")
  return ([pid])
@

-}
module System.Prefork(
    module System.Prefork.Class
  , module System.Prefork.Types
  , module System.Prefork.Main
  , module System.Prefork.Worker
  , module System.Prefork.Settings
  ) where

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Main
import System.Prefork.Worker
import System.Prefork.Settings

-- Copyright: (c) 2013 Gree, Inc.
-- License: MIT-style

import System.Prefork
import System.Posix
import System.Exit (exitSuccess)

data ServerConfig = ServerConfig
data Worker = Worker1 String deriving (Show, Read)

instance WorkerContext Worker

main :: IO ()
main = defaultMain defaultSettings {
    psUpdateConfig = updateConfig
  , psUpdateServer = updateServer
  , psOnStart      = \_ -> do
      pid <- getProcessID
      putStrLn $ "Please send SIGHUP to " ++ show pid ++ " to relaunch a worker"
  } $ \so -> case so of
  Worker1 msg -> print msg >> exitSuccess

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (Worker1 "Hello. I'm a worker.")
  return ([pid])

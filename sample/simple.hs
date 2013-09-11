
{-# LANGUAGE ScopedTypeVariables #-}

import System.Prefork
import System.Posix

data ServerConfig = ServerConfig
data ServerOption = ServerOption1 String deriving (Show, Read)

instance WorkerContext ServerOption

main :: IO ()
main = do
  defaultMain defaultSettings {
      psUpdateConfig = updateConfig
    , psUpdateServer = updateServer
    } $ \so -> case so of
    ServerOption1 msg -> do
      print msg

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (ServerOption1 "Hello. I'm a worker.")
  return ([pid])

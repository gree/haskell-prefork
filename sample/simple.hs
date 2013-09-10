
{-# LANGUAGE ScopedTypeVariables #-}

import System.Prefork
import Network.Socket

data ServerResource = ServerResource [Socket]

data ServerConfig = ServerConfig
data ServerOption = ServerOption1 deriving (Show, Read)

instance WorkerContext ServerOption

main :: IO ()
main = do
  defaultMain defaultSettings { psUpdateConfig = updateConfig } $ \(so :: ServerOption) -> return ()

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

newServerResource = ServerResource []

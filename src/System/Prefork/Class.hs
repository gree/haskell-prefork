
module System.Prefork.Class where

class (Show a, Read a) => WorkerContext a where
  encodeToString :: a -> String
  encodeToString = show
  
  decodeFromString :: String -> a
  decodeFromString = read

  rtsOptions :: a -> [String]
  rtsOptions _ = []

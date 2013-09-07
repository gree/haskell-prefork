
import System.Prefork

data ServerOption = ServerOption1 deriving (Show, Read)

main :: IO ()
main = do
  defaultMain ServerOption1 readConfig

readConfig ::  IO (ServerOption, String)
readConfig = do
  return (ServerOption1, "")
  

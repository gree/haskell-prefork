
import System.Prefork
import Network.Socket

data ServerResource = ServerResource [Socket]

data ServerOption = ServerOption1 deriving (Show, Read)

main :: IO ()
main = do
  defaultMain PreforkSettings {
    psTerminateHandler = defaultTerminateHandler
  , psInterruptHandler = defaultInterruptHandler
  , psHungupHandler    = defaultHungupHandler
  , psChildHandler     = defaultChildHandler
  , psReadConfigFn     = readConfig
                              } $ do
    \so -> return ()

readConfig :: IO (ServerOption, String)
readConfig = do
  return (ServerOption1, "")

newServerResource = ServerResource []

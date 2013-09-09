
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
  , psUpdateConfigFn   = updateConfig
                              } $ do
    \so -> return ()

updateConfig :: IO (ServerOption, String)
updateConfig = do
  return (ServerOption1, "")

newServerResource = ServerResource []

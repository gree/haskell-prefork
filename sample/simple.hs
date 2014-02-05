
import System.Prefork
import System.Posix

data ServerConfig = ServerConfig
data Worker = Worker1 String deriving (Show, Read)

instance WorkerContext Worker

main :: IO ()
main = do
  defaultMain defaultSettings {
      psUpdateConfig = updateConfig
    , psUpdateServer = updateServer
    } $ \so -> case so of
    Worker1 msg -> do
      print msg

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (Worker1 "Hello. I'm a worker.")
  return ([pid])

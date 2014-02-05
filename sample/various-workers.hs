
import System.Prefork
import System.Posix
import System.Exit (exitSuccess)

data ServerConfig = ServerConfig
data Worker = Worker1 String | Worker2 String deriving (Show, Read, Eq)

instance WorkerContext Worker

main :: IO ()
main = defaultMain defaultSettings {
    psUpdateConfig = updateConfig
  , psUpdateServer = updateServer
  , psOnStart      = \_ -> do
      pid <- getProcessID
      putStrLn $ "Please send SIGHUP to " ++ show pid ++ " to relaunch workers"
  } $ \w -> case w of
  Worker1 msg -> putStrLn msg >> exitSuccess
  Worker2 msg -> putStrLn msg >> exitSuccess

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid1 <- forkWorkerProcess (Worker1 "Hello. I'm a worker 1.")
  pid2 <- forkWorkerProcess (Worker2 "Hello. I'm a worker 2.")
  return ([pid1, pid2])

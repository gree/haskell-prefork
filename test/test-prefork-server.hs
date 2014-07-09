import System.Prefork
import System.Posix
import System.Exit (exitSuccess)
import System.IO 
import System.Environment
import Control.Monad
import Control.Concurrent

import Constant (
    Worker(..)
  , workerNum
  , serverOption
  , masterOutputFile
  , workerOutputFile 
  , settingDefault
  , settingRelaunch
  )

data ServerConfig = ServerConfig

main :: IO ()
main = do
  -- not to run in cabal test
  args <- getArgs
  case args of
    ("keep-alive":_) -> runServer
    _ -> exitSuccess

runServer :: IO ()
runServer = do
    args <- getArgs
    case args of
      ("keep-alive":settings:_) -> case settings of
        "defaultSettings" -> runByDefaultSettings
        "relaunchSettings" -> runByRelaunchSettings
        _ -> exitSuccess
      _ -> exitSuccess



runByDefaultSettings :: IO ()
runByDefaultSettings = defaultMain defaultSettings {
        psOnTerminate      = onTerminate
      , psOnInterrupt      = onInterrupt
      , psOnQuit           = onQuit
      , psOnChildFinished  = \_config -> return ([])
      , psOnStart      = onStart
      , psOnFinish     = onFinish
      , psUpdateServer = updateServer
      , psCleanupChild     = \_config _pid -> return ()
      , psUpdateConfig = updateConfig

      } $ \so -> case so of
        Worker msg -> withFile workerOutputFile WriteMode $ \hdl -> do 
          hPutStr hdl msg
          exitSuccess
  where
    onTerminate :: ServerConfig -> [ProcessID] -> IO ()
    onTerminate _ cids = do
      forM_ cids $ \cid -> signalProcess sigTERM cid

    onInterrupt :: ServerConfig -> [ProcessID] -> IO ()
    onInterrupt _ cids = do
      forM_ cids $ \cid -> signalProcess sigINT cid

    onQuit :: ServerConfig -> IO ()
    onQuit _ = do
      withFile workerOutputFile WriteMode $ \hdl -> do
        hPutStr hdl "onQuit"

    onStart :: Maybe ServerConfig -> IO ()
    onStart _ = do
      withFile masterOutputFile WriteMode $ \hdl -> do
        hPutStr hdl "onStart"

    onFinish :: Maybe ServerConfig -> IO ()
    onFinish _ = do
      withFile masterOutputFile WriteMode $ \hdl -> do
        hPutStr hdl "onFinish"

    updateServer :: ServerConfig -> IO ([ProcessID])
    updateServer ServerConfig = do
      pid <- forkWorkerProcessWithArgs (Worker "updateServer") [serverOption, settingDefault]
      return ([pid])

    updateConfig :: IO (Maybe ServerConfig)
    updateConfig = do
      return (Just ServerConfig)


runByRelaunchSettings :: IO ()
runByRelaunchSettings = do
  resource <- makePreforkResource []
  defaultMain (relaunchSettings resource updateAction forkAction) $ \(Worker _) -> do
    threadDelay 10000000000

  where
    updateAction :: PreforkResource Worker -> IO (Maybe ServerConfig)
    updateAction resource = do
      updateWorkerSet resource $ flip map [0..(workerNum-1)] $ \i ->
        Worker $ "worker" ++ show i
      return (Just ServerConfig)

    forkAction :: Worker -> IO (ProcessID)
    forkAction worker = do
      pid <- forkWorkerProcessWithArgs worker [serverOption, settingRelaunch]
      withFile "/tmp/relaunch_workers" AppendMode $ \hdl -> do
        hPutStrLn hdl $ show pid
      return pid



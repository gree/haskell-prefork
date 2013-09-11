
module System.Prefork.Worker (
    forkWorkerProcess
  , workerMain
  ) where

import Control.Monad
import Control.Exception
import System.Process
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import Filesystem.Path.CurrentOS(encodeString)
import System.Posix hiding (version)
import Foreign.C.Types
import System.IO (hPutStrLn)
import System.Exit
import System.Argv0
import Data.Maybe

import System.Prefork.Class

{- |
-}
forkWorkerProcess :: (WorkerContext so) => so -> IO ProcessID
forkWorkerProcess opt = do
  exe <- liftM encodeString getArgv0
  let options = ["server"] ++ case rtsOptions opt of
        [] -> []
        rtsopt -> ["+RTS"] ++ rtsopt ++ ["-RTS"]
  (jhin, _, _, ph) <- createProcess $ (proc exe options) { std_in = CreatePipe }
  hPutStrLn (fromJust jhin) $ encodeToString opt
  extractProcessID ph

extractProcessID :: ProcessHandle -> IO ProcessID
extractProcessID h = withProcessHandle h $ \x -> case x of
  OpenHandle pid -> return (x, pid)
  _ -> throwIO $ userError "Unable to retrieve child process ID."

{- |
-}
workerMain :: (WorkerContext so) => (so -> IO ()) -> IO ()
workerMain act = do
  rawOpt <- getLine
  act $ decodeFromString rawOpt
  exitSuccess

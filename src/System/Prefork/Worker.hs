
module System.Prefork.Worker where

import Control.Monad
import Control.Exception
import System.Process
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import Filesystem.Path.CurrentOS(encodeString)
import System.Posix hiding (version)
import Foreign.C.Types
import System.IO (hPrint)
import System.Exit
import System.Argv0
import Data.Maybe


forkServerProcess :: (Read so, Show so) => so -> IO ProcessID
forkServerProcess opt = do
  exe <- liftM encodeString getArgv0
  let options = ["server"]
  (jhin, _, _, ph) <- createProcess $ (proc exe options) { std_in = CreatePipe }
  hPrint (fromJust jhin) $ opt
  extractProcessID ph

extractProcessID :: ProcessHandle -> IO ProcessID
extractProcessID h = withProcessHandle h $ \x -> case x of
  OpenHandle pid -> return (x, pid)
  _ -> throwIO $ userError "Unable to retrieve child process ID."


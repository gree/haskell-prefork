
{-# LANGUAGE ScopedTypeVariables #-}

module System.Prefork.Worker (
    forkWorkerProcess
  , forkWorkerProcessWithArgs
  , workerMain
  , preforkEnvKey
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
import System.Environment (lookupEnv)
import System.IO
import Control.Concurrent

import System.Prefork.Class

preforkEnvKey :: String
preforkEnvKey = "PREFORK"

{- |
-}
forkWorkerProcessWithArgs :: (WorkerContext so) => so -> [String] -> IO ProcessID
forkWorkerProcessWithArgs opt args = do
  exe <- liftM encodeString getArgv0
  mPrefork <- lookupEnv preforkEnvKey
  let heads = case mPrefork of
        Just _ -> []
        Nothing -> ["server"]
  let options = case rtsOptions opt of
        [] -> args
        rtsopt -> ["+RTS"] ++ rtsopt ++ ["-RTS"] ++ args
  (Just hIn, Just hOut, _, ph) <- createProcess $ (proc exe (heads ++ options)) { std_in = CreatePipe, std_out = CreatePipe }
  forkIO $ hPutStr stdout =<< hGetContents hOut
  hPutStrLn hIn $ encodeToString opt
  extractProcessID ph
  where
    extractProcessID :: ProcessHandle -> IO ProcessID
    extractProcessID h = withProcessHandle h $ \x -> case x of
      OpenHandle pid -> return (x, pid)
      _ -> throwIO $ userError "Unable to retrieve child process ID."

{- |
-}
forkWorkerProcess :: (WorkerContext so) => so -> IO ProcessID
forkWorkerProcess opt = forkWorkerProcessWithArgs opt []

{- |
-}
workerMain :: (WorkerContext so) => (so -> IO ()) -> IO ()
workerMain act = do
  rawOpt <- getLine
  act $ decodeFromString rawOpt
  exitSuccess

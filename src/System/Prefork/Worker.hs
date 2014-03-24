-- Copyright: (c) 2013 GREE, Inc.
-- License: MIT-style

{-# LANGUAGE ScopedTypeVariables #-}

module System.Prefork.Worker (
    forkWorkerProcess
  , forkWorkerProcessWithArgs
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
import System.Argv0
import Data.Maybe
import System.Environment (lookupEnv)
import System.IO
import Control.Concurrent

import System.Prefork.Class

preforkEnvKey :: String
preforkEnvKey = "PREFORK"

{- | create a new worker with arguments
-}
forkWorkerProcessWithArgs :: (WorkerContext a)
                             => a            -- ^ a worker context
                             -> [String]     -- ^ command line arguments
                             -> IO ProcessID -- ^ a process id of a created worker
forkWorkerProcessWithArgs opt args = do
  exe <- liftM encodeString getArgv0
  mPrefork <- lookupEnv preforkEnvKey
  (Just hIn, Just hOut, _, ph) <- createProcess $ (proc exe options) { std_in = CreatePipe, std_out = CreatePipe }
  forkIO $ hPutStr stdout =<< hGetContents hOut
  hPutStrLn hIn $ encodeToString opt
  extractProcessID ph
  where
    options :: [String]
    options = case rtsOptions opt of
      [] -> args
      rtsopts -> args ++ ["+RTS"] ++ rtsopts ++ ["-RTS"]

    extractProcessID :: ProcessHandle -> IO ProcessID
    extractProcessID h = withProcessHandle h $ \x -> case x of
      OpenHandle pid -> return pid
      _ -> throwIO $ userError "Unable to retrieve child process ID."

{- | create a new worker
-}
forkWorkerProcess :: (WorkerContext a)
                     => a            -- ^ a worker context
                     -> IO ProcessID -- ^ a process id of a created worker
forkWorkerProcess opt = forkWorkerProcessWithArgs opt []


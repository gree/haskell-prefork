{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import System.Posix
import System.Process
import System.Process.Internals (withProcessHandle, ProcessHandle__(OpenHandle))
import System.IO
import System.Directory
import System.FilePath
import Data.Functor

import Util
import System.Prefork.Class
import System.Prefork.Types

import Constant (
    Worker(..)
  , workerNum
  , serverOption
  , masterOutputFile
  , workerOutputFile 
  , relaunchWorkerFile
  , settingDefault
  , settingRelaunch
  ) 


main :: IO ()
main = do
  hspec $ do
    describe "Class" $ do
      let worker = Worker "test"
      it "translate worker to string" $ encodeToString worker `shouldBe` show worker
      it "translate string to worker" $ decodeFromString "Worker \"test\"" `shouldBe` worker
      it "returns default options" $ rtsOptions worker `shouldBe` []

    describe "Types" $ do
      let w1 = Worker "test1"
          w2 = Worker "test2"

      it "makes PreforkResource" $ do
        resource <- makePreforkResource [w1, w2]
        workerMap <- atomically $ readTVar $ prProcs resource
        workerSet <- atomically $ readTVar $ prWorkers resource
        M.size workerMap `shouldBe` 0
        workerSet `shouldBe` S.fromList [w1, w2]

      it "updates workers" $ do
        resource <- makePreforkResource [w1, w2]
        updateWorkerSet resource [w1]
        workerSet <- atomically $ readTVar $ prWorkers resource
        workerSet `shouldBe` S.fromList [w1]

    describe "Main" $ do
      it "makes test server" $ do
        (_, ph) <- createTestServer settingDefault
        withFile masterOutputFile ReadMode $ \hdl -> do
          threadDelay 1000000
          flip shouldBe "onStart" =<< hGetContents hdl
        terminateProcess ph

      it "sends sigHUP" $ do
        checkOutputOnSignal sigHUP workerOutputFile "updateServer"

      it "sends sigTERM" $ do
        checkOutputOnSignal sigTERM masterOutputFile "onFinish"

      it "sends sigINT" $ do
        checkOutputOnSignal sigINT masterOutputFile "onFinish"

      it "sends sigQUIT" $ do
        checkOutputOnSignal sigQUIT workerOutputFile "onQuit"


      it "sends sigHUP to relauch settings server" $ do
        (pid, ph) <- createTestServer settingRelaunch
        testActionBySignal sigHUP pid  relaunchWorkerFile $ \hdl -> do
          terminateProcess ph
          workerPids <- lines <$> hGetContents hdl
          length workerPids `shouldBe` workerNum

      it "sends sigTERM to worker in relauch settings" $ do
        writeFile relaunchWorkerFile ""
        (_, ph) <- createTestServer settingRelaunch
        h <- openFile relaunchWorkerFile ReadMode
        workerPid <- hGetLine h
        hClose h
        testActionBySignal sigTERM (read workerPid) relaunchWorkerFile $ \hdl -> do
          terminateProcess ph
          workerPids <- lines <$> hGetContents hdl
          length workerPids `shouldBe` 1

createTestServer :: String -> IO (ProcessID, ProcessHandle)
createTestServer settings = do
      cDir <- getCurrentDirectory
      distDir <- getDistDir cDir
      let exePath = cDir </> distDir </> "build" </> "test-prefork-server" </> "test-prefork-server"
      (_, Just hOut, _, ph) <- createProcess $ (proc exePath [serverOption, settings]) { std_out = CreatePipe }
      _ <- forkIO $ hPutStr stdout =<< hGetContents hOut
      pid <- withProcessHandle ph $ \x -> case x of
        OpenHandle pid' -> return pid'
        _ -> throwIO $ userError "Unable to retrieve child process ID."

      threadDelay 1000000
      return (pid, ph)

checkOutputOnSignal :: Signal -> String -> String -> IO ()
checkOutputOnSignal sig file expected = do
  (pid, ph) <- createTestServer settingDefault
  testActionBySignal sig pid file $ \hdl -> do
    flip shouldBe expected =<< hGetContents hdl
  terminateProcess ph


testActionBySignal :: Signal -> ProcessID -> String -> (Handle -> IO ()) -> IO ()
testActionBySignal sig pid file testAction = do
  writeFile file ""
  signalProcess sig pid
  withFile file ReadMode $ \hdl -> do
    threadDelay 1000000
    testAction hdl

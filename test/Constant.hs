module Constant (
    Worker(..)
  , workerNum
  , serverOption
  , masterOutputFile
  , workerOutputFile 
  , relaunchWorkerFile
  , settingDefault
  , settingRelaunch
) where

import System.Prefork.Class


data Worker = Worker String deriving (Eq, Ord, Show, Read)

instance WorkerContext Worker

workerNum :: Int
workerNum = 2

serverOption :: String
serverOption = "keep-alive"

masterOutputFile :: String
masterOutputFile = "/tmp/master"

workerOutputFile :: String
workerOutputFile = "/tmp/worker"

relaunchWorkerFile :: String
relaunchWorkerFile = "/tmp/relaunch_workers"

settingDefault :: String
settingDefault = "defaultSettings"

settingRelaunch :: String
settingRelaunch = "relaunchSettings"


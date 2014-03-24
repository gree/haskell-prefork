{- |
  Module      : System.Prefork
  Copyright   : (c) 2013 Gree, Inc.
  License     : MIT-style
  
  Maintainer  : Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>
  Stability   : experimental
  Portability : portable
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module System.Prefork(
  -- * Overview
  -- $overview
  
  -- * Tutorial
  -- ** A typical preforking server
  -- $relaunch_tutorial
  
  -- ** Using low-level interface
  -- $rawlevel_tutorial
    module System.Prefork.Class
  , module System.Prefork.Types
  , module System.Prefork.Main
  , module System.Prefork.Worker
  , module System.Prefork.Settings
  ) where

import System.Prefork.Class
import System.Prefork.Types
import System.Prefork.Main
import System.Prefork.Worker
import System.Prefork.Settings

{- $overview
This is a library for servers based on worker process model (preforking).
-}

{- $relaunch_tutorial
Import System.Prefork in your Main module.

@
import ...
import System.Posix
import System.Prefork
import System.Console.CmdArgs
@

Define data type used for server configuration.

@
-- Application specific configuration
data Config = Config {
    cWarpSettings :: Warp.Settings
  }
@

Define workers as a data type that belongs to 'WorkerContext' class.
In this case, the field 'wId' is a ID number for idenfitying single worker process and other fields are
parameters for a worker process.

@
-- Worker context passed by the parent
data Worker = Worker {
    wId       :: Int
  , wPort     :: Int
  , wSocketFd :: CInt
  , wHost     :: String
  , wCap      :: Int
  } deriving (Show, Read)

instance WorkerContext Worker where
  rtsOptions w = [\"-N\" ++ show (wCap w)]
@

Define Eq and Ord instances for Worker. These are required for using 'relaunchSettings'.

@
instance Eq Worker where
  (==) a b = wId a == wId b

instance Ord Worker where
  compare a b = compare (wId a) (wId b)
@

Call 'defaultMain' with 'update' and 'fork' functions in your 'main' function.
'relaunchSettings' is a function that creates comvenient settings for a typical prefork server.

@
main :: IO ()
main = do
  option <- cmdArgs cmdLineOptions
  resource <- emptyPreforkResource
  mSoc <- newTVarIO Nothing
  let s = Server mSoc (port option) (workers option)
  defaultMain (relaunchSettings resource (update s) (fork s)) $ \(Worker { wId = i, wSocketFd = fd, wHost = _host }) -> do
    -- worker action
    soc <- mkSocket fd AF_INET Stream defaultProtocol Listening
    mConfig <- updateConfig s
    case mConfig of
      Just config -> do
        a <- asyncOn i $ Warp.runSettingsSocket (cWarpSettings config) soc $ serverApp
        wait a
      Nothing -> return ()
  where
    ...
@

'update' function is used for modifying the worker process configuration.
If you want to increase or decrease the number of workers, change worker parameters, and etc,
you can use 'updateWorkerSet' function here.

@
    update :: Server -> PreforkResource Worker -> IO (Maybe Config)
    update s resource = do
      mConfig <- updateConfig s
      updateWorkerSet resource $ flip map [0..(sWorkers s - 1)] $ \i ->
        Worker { wId = i, wPort = (sPort s), wSocketFd = -1, wHost = "localhost", wCap = sWorkers s }
      return (mConfig)
@

'fork' function simply creates a worker process with the given parameters.

@
    fork :: Server -> Worker -> IO (ProcessID)
    fork Server { sServerSoc = socVar } w = do
      msoc <- readTVarIO socVar
      soc <- case msoc of
        Just soc -> return (soc)
        Nothing -> do
          hentry <- getHostByName (wHost w)
          soc <- listenOnAddr (SockAddrInet (fromIntegral (wPort w)) (head $ hostAddresses hentry))
          atomically $ writeTVar socVar (Just soc)
          return (soc)
      let w' = w { wSocketFd = fdSocket soc }
      forkWorkerProcessWithArgs (w') ["id=" ++ show (wId w') ]
@

-}

{- $rawlevel_tutorial
Import System.Prefork in your Main module.

@
import System.Prefork
import System.Posix
import System.Exit (exitSuccess)
@

Define data type used for server configuration.

@
data ServerConfig = ServerConfig
@

Define workers as a data type.

@
data Worker = Worker1 String deriving (Show, Read)

instance WorkerContext Worker
@

Call System.Prefork.defaultMain function with settings in your main function.
 
@
main :: IO ()
main = defaultMain defaultSettings {
    psUpdateConfig = updateConfig
  , psUpdateServer = updateServer
  , psOnStart      = \_ -> do
      pid <- getProcessID
      putStrLn $ \"Please send SIGHUP to \" ++ show pid ++ \" to relaunch a worker\"
  } $ \so -> case so of
  Worker1 msg -> print msg >> exitSuccess

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (Worker1 \"Hello. I'm a worker.\")
  return ([pid])
@

-}

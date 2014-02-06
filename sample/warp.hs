
{-# LANGUAGE OverloadedStrings #-}

-- This is a simple web server based on Warp

import Blaze.ByteString.Builder.Char.Utf8
import Foreign.C.Types
import Network.BSD
import Network.Socket
import Control.Applicative
import Control.Exception
import Control.Concurrent.STM
import Control.Monad
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types
import System.Posix
import System.Prefork
import System.Console.CmdArgs
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

-- Application specific configuration
data Config = Config {
    cWarpSettings :: Warp.Settings
  , cPort         :: Int
  , cHost         :: String
  }

-- Worker context passed by the parent
data Worker = Worker {
    wId       :: Int
  , wSocketFd :: CInt
  , wHost     :: String
  } deriving (Show, Read)

instance WorkerContext Worker where
  rtsOptions _ = ["-N4"]

instance Ord Worker where
  compare a b = case (a, b) of
    (Worker { wId = aid }, Worker { wId = bid }) -> compare aid bid

instance Eq Worker where
  (==) a b = compare a b == EQ

-- Server states
data Server = Server {
    sResource  :: PreforkResource Worker
  , sServerSoc :: TVar (Maybe Socket)
  , sPort      :: Int
  , sWorkers   :: Int
  }

-- Command line options
data Warp = Warp {
    port      :: Int
  , workers   :: Int
  , extraArgs :: [String]
  } deriving (Show, Data, Typeable, Eq)

cmdLineOptions :: Warp
cmdLineOptions = Warp {
      port      = 11111 &= name "p" &= help "Port number" &= typ "PORT"
    , workers   = 4 &= name "w" &= help "Number of workers" &= typ "NUM"
    , extraArgs = def &= args
    } &=
    help "Preforking Warp Server Sample" &=
    summary ("Preforking Warp Server Sample, (C) GREE, Inc") &=
    details ["Web Server"]

-- Call defaultMain
main :: IO ()
main = do
  option <- cmdArgs cmdLineOptions
  s <- Server <$> makePreforkResource []
              <*> newTVarIO Nothing
              <*> pure (port option)
              <*> pure (workers option)
  let settings = defaultSettings {
      psUpdateConfig = updateConfig s
    , psUpdateServer = updateServer s
    , psCleanupChild = cleanupChild s
    , psOnChildFinished = relaunchWorkers s
    }
  defaultMain settings $ \(Worker { wSocketFd = fd, wHost = _host }) -> do
    -- worker action
    soc <- mkSocket fd AF_INET Stream defaultProtocol Listening
    mConfig <- updateConfig s
    case mConfig of
      Just config -> Warp.runSettingsSocket (cWarpSettings config) soc $ serverApp
      Nothing -> return ()
  where
    serverApp :: Application
    serverApp _ = return $ ResponseBuilder status200 [] $ fromString "hello"

-- Load settings via IO
updateConfig :: Server -> IO (Maybe Config)
updateConfig s = do
  let workers = map (\i -> Worker { wId = i, wSocketFd = 0, wHost = "" }) [1..(sWorkers s)]
  atomically $ writeTVar (prWorkers $ sResource s) $ S.fromList workers
  return (Just $ Config Warp.defaultSettings { Warp.settingsPort = fromIntegral (sPort s) } (sPort s) "localhost")

-- Update the entire state of a server
updateServer :: Server -> Config -> IO ([ProcessID])
updateServer Server { sServerSoc = socVar, sResource = resource } Config { cHost = host, cPort = listenPort } = do
  let procs = prProcs resource
  msoc <- readTVarIO socVar
  soc <- case msoc of
    Just soc -> return (soc)
    Nothing -> do
      hentry <- getHostByName host
      soc <- listenOnAddr (SockAddrInet (fromIntegral listenPort) (head $ hostAddresses hentry))
      atomically $ writeTVar socVar (Just soc)
      return (soc)
  workers <- readTVarIO (prWorkers resource)
  newPids <- forM (S.toList workers) $ \w -> do
    let w' = w { wSocketFd = fdSocket soc, wHost = host }
    pid <- forkWorkerProcessWithArgs (w') ["id=" ++ show (wId w') ]
    return (pid, w)
  oldPids <- atomically $ swapTVar procs (M.fromList newPids)
  forM_ (M.keys oldPids) $ sendSignal sigTERM
  return (map fst newPids)

-- Clean up application specific resources associated to a child process
cleanupChild :: Server -> Config -> ProcessID -> IO ()
cleanupChild Server { sResource = resource } _config pid = do
  let procs = prProcs resource
  atomically $ modifyTVar' procs $ M.delete pid

-- Relaunch workers after some of them terminate
relaunchWorkers :: Server -> Config -> IO ([ProcessID])
relaunchWorkers Server { sResource = resource } _config = do
  let procs = prProcs resource
  let workers = prWorkers resource
  live <- readTVarIO procs
  workers <- readTVarIO workers
  newPids <- fmap M.fromList $ forM (S.toList workers \\ M.elems live) $ \w -> do
    pid <- forkWorkerProcessWithArgs w ["id=" ++ show (wId w) ]
    return (pid, w)
  atomically $ modifyTVar' procs $ M.union newPids
  return (M.keys newPids)

-- Create a server socket with SockAddr
listenOnAddr :: SockAddr -> IO Socket
listenOnAddr sockAddr = do
  let backlog = 1024
  proto <- getProtocolNumber "tcp"
  bracketOnError
    (socket AF_INET Stream proto)
    (sClose)
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bindSocket sock sockAddr
      listen sock backlog
      return sock
    )

-- Send a signal
sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()


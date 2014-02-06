
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

-- Application specific configuration
data Config = Config {
    cWarpSettings :: Warp.Settings
  , cPort         :: Int
  , cHost         :: String
  , cWorkers      :: Int
  }

-- Worker context passed by the parent
data Worker = Worker {
    wSocketFd :: CInt
  , wHost     :: String
  } deriving (Show, Read)

instance WorkerContext Worker where
  rtsOptions _ = ["-N4"]

-- Server states
data Server = Server {
    sServerSoc :: TVar (Maybe Socket)
  , sProcs     :: TVar [ProcessID]
  , sPort      :: Int
  }

-- Command line options
data Warp = Warp {
    port      :: Int
  , extraArgs :: [String]
  } deriving (Show, Data, Typeable, Eq)

cmdLineOptions :: Warp
cmdLineOptions = Warp {
      port      = 11111 &= name "p" &= help "Port number" &= typ "PORT"
    , extraArgs = def &= args
    } &=
    help "Preforking Warp Server Sample" &=
    summary ("Preforking Warp Server Sample, (C) GREE, Inc") &=
    details ["Web Server"]

-- Call defaultMain
main :: IO ()
main = do
  option <- cmdArgs cmdLineOptions
  s <- Server <$> newTVarIO Nothing <*> newTVarIO [] <*> pure (port option)
  let settings = defaultSettings {
      psUpdateConfig = updateConfig s
    , psUpdateServer = updateServer s
    , psCleanupChild = cleanupChild s
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
updateConfig s = return (Just $ Config Warp.defaultSettings { Warp.settingsPort = fromIntegral (sPort s) } (sPort s) "localhost" 10)

-- Update the entire state of a server
updateServer :: Server -> Config -> IO ([ProcessID])
updateServer Server { sServerSoc = socVar, sProcs = procs } Config { cHost = host, cPort = listenPort, cWorkers = workers } = do
  msoc <- readTVarIO socVar
  soc <- case msoc of
    Just soc -> return (soc)
    Nothing -> do
      hentry <- getHostByName host
      soc <- listenOnAddr (SockAddrInet (fromIntegral listenPort) (head $ hostAddresses hentry))
      atomically $ writeTVar socVar (Just soc)
      return (soc)
  newPids <- replicateM workers $ forkWorkerProcess (Worker { wSocketFd = fdSocket soc, wHost = host })
  oldPids <- atomically $ swapTVar procs newPids
  forM_ oldPids $ sendSignal sigTERM
  return (newPids)

-- Clean up application specific resources associated to a child process
cleanupChild :: Server -> Config -> ProcessID -> IO ()
cleanupChild Server { sProcs = procs } _config pid = do
  atomically $ modifyTVar' procs $ filter (/= pid)

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
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()


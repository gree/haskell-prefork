
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , wPort :: Int
  , wHost :: String
  } deriving (Show, Read)

instance WorkerContext Worker

-- Server states
data Server = Server {
    sServerSoc :: TVar (Maybe Socket)
  , sProcs :: TVar [ProcessID]
  }

-- Call defaultMain or compatMain
main :: IO ()
main = do
  s <- Server <$> newTVarIO Nothing <*> newTVarIO []
  let settings = defaultSettings {
      psUpdateConfig = updateConfig
    , psUpdateServer = updateServer s
    , psCleanupChild = cleanupChild s
    }
  compatMain settings $ \(Worker { wSocketFd = fd, wPort = port, wHost = _host }) -> do
    -- worker action
    soc <- mkSocket fd AF_INET Stream defaultProtocol Listening
    Warp.runSettingsSocket Warp.defaultSettings {
        Warp.settingsPort = fromIntegral port
      } soc $ serverApp
  where
    serverApp :: Application
    serverApp _ = return $ ResponseBuilder status200 [] $ fromString "hello"

-- Load settings via IO
updateConfig :: IO (Maybe Config)
updateConfig = return (Just $ Config Warp.defaultSettings 11111 "localhost" 10)

-- Update the entire state of a server
updateServer :: Server -> Config -> IO ([ProcessID])
updateServer Server { sServerSoc = socVar, sProcs = procs } Config { cHost = host, cPort = port, cWorkers = workers } = do
  msoc <- readTVarIO socVar
  soc <- case msoc of
    Just soc -> return (soc)
    Nothing -> do
      hentry <- getHostByName host
      soc <- listenOnAddr (SockAddrInet (fromIntegral port) (head $ hostAddresses hentry))
      atomically $ writeTVar socVar (Just soc)
      return (soc)
  newPids <- replicateM workers $ forkWorkerProcess (Worker { wSocketFd = fdSocket soc, wHost = host, wPort = port })
  oldPids <- atomically $ do
    oldPids <- readTVar procs
    writeTVar procs newPids
    return (oldPids)
  forM_ oldPids $ sendSignal sigTERM
  return (newPids)

-- Clean up application specific resources associated to a child process
cleanupChild :: Server -> Config -> ProcessID -> IO ()
cleanupChild Server { sProcs = procs } _config pid = do
  atomically $ modifyTVar' procs $ filter (/= pid)
  return ()

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


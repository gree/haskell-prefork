
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Blaze.ByteString.Builder.Char.Utf8
import System.Posix
import System.Prefork

data Config = Config {
    cWarpSettings :: Warp.Settings
  , cPort :: Int
  , cHost :: String
  }

data Worker = Worker {
    wSocketFd :: CInt
  , wPort :: Int
  , wHost :: String
  } deriving (Show, Read)

instance WorkerContext Worker

data Server = Server {
    sServerSoc :: TVar (Maybe Socket)
  , sProcs :: TVar [ProcessID]
  }

main :: IO ()
main = do
  s <- Server <$> newTVarIO Nothing <*> newTVarIO []
  let settings = defaultSettings {
      psUpdateConfig = updateConfig
    , psUpdateServer = updateServer s
    , psCleanupChild = cleanupChild s
    }
  compatMain settings $ \(Worker { wSocketFd = fd}) -> do
    soc <- mkSocket fd AF_INET Stream defaultProtocol Listening
    sockAddr <- getSocketName soc
    case sockAddr of
      SockAddrInet port _addr -> do
        Warp.runSettingsSocket Warp.defaultSettings {
            Warp.settingsPort = fromIntegral port
          } soc $ serverApp
        return ()
      _ -> return ()
    return ()
  where
    serverApp :: Application
    serverApp _ = return $ ResponseBuilder status200 [] $ fromString "hello"

updateConfig :: IO (Maybe Config)
updateConfig = do
  let settings = Warp.defaultSettings
  return (Just $ Config settings 11111 "localhost")

updateServer :: Server -> Config -> IO ([ProcessID])
updateServer Server { sServerSoc = socVar, sProcs = procs } Config { cHost = host, cPort = port } = do
  msoc <- readTVarIO socVar
  soc <- case msoc of
    Just soc -> return (soc)
    Nothing -> do
      hentry <- getHostByName host
      soc <- listenOnAddr (SockAddrInet (fromIntegral port) (head $ hostAddresses hentry))
      atomically $ writeTVar socVar (Just soc)
      return (soc)
  newPids <- forM [1..10] $ \(_ :: Int) -> forkWorkerProcess (Worker { wSocketFd = fdSocket soc, wHost = host, wPort = port })
  oldPids <- atomically $ do
    oldPids <- readTVar procs
    writeTVar procs newPids
    return (oldPids)
  forM_ oldPids $ sendSignal sigTERM
  return (newPids)

cleanupChild :: Server -> Config -> ProcessID -> IO ()
cleanupChild Server { sProcs = procs } _config pid = do
  atomically $ modifyTVar' procs $ filter (/= pid)
  return ()

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

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()


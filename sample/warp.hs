
{-# LANGUAGE OverloadedStrings #-}

-- This is a simple web server based on Warp

import Data.Maybe
import Blaze.ByteString.Builder.Char.Utf8
import Foreign.C.Types
import Network.BSD
import Network.Socket
import Control.Monad
import Control.Exception
import Control.Concurrent.STM
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types
import System.Posix
import System.Prefork
import System.Console.CmdArgs
import GHC.Conc

-- Application specific configuration
data Config = Config {
    cWarpSettings :: Warp.Settings
  }

-- Worker context passed by the parent
data Worker = Worker {
    wId       :: Int
  , wPort     :: Int
  , wSocketFd :: CInt
  , wHost     :: String
  , wCap      :: Int
  } deriving (Show, Read)

instance WorkerContext Worker where
  rtsOptions w = ["-N" ++ show (wCap w)]

instance Ord Worker where
  compare a b = case (a, b) of
    (Worker { wId = aid }, Worker { wId = bid }) -> compare aid bid

instance Eq Worker where
  (==) a b = compare a b == EQ

-- Server states
data Server = Server {
    sServerSoc :: TVar (Maybe Socket)
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

main :: IO ()
main = do
  option <- cmdArgs cmdLineOptions
  resource <- makePreforkResource []
  mSoc <- newTVarIO Nothing
  let s = Server mSoc (port option) (workers option)
  defaultMain (relaunchSettings resource (update s) (fork s)) $ \(Worker { wId = i, wSocketFd = fd, wHost = _host }) -> do
    -- worker action
    soc <- mkSocket fd AF_INET Stream defaultProtocol Listening
    mConfig <- update s resource
    case mConfig of
      Just config -> do
        v <- newTVarIO Nothing
        _ <- forkOn i $ do
          Warp.runSettingsSocket (cWarpSettings config) soc $ serverApp
          atomically $ writeTVar v (Just ())
        atomically $ do
          x <- readTVar v
          when (isNothing x) retry
        return ()
      Nothing -> return ()
  where
    update :: Server -> PreforkResource Worker -> IO (Maybe Config)
    update s resource = do
      updateWorkerSet resource $ flip map [0..(sWorkers s - 1)] $ \i ->
        Worker { wId = i, wPort = (sPort s), wSocketFd = -1, wHost = "localhost", wCap = sWorkers s }
      updateConfig s

    updateConfig :: Server -> IO (Maybe Config)
    updateConfig s = return (Just $ Config Warp.defaultSettings { Warp.settingsPort = fromIntegral (sPort s) })

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

-- Web application
serverApp :: Application
serverApp _ = return $ ResponseBuilder status200 [] $ fromString "hello"

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



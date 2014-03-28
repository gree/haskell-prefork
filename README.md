
Haskell Prefork
===============

haskell-prefork is a library for preforking daemons.

How to install
--------------

Execute the cabal install command and the library and example programs will be placed into your .cabal directory.

    > git clone .../haskell-prefork.git
    > cd haskell-prefork/
    > cabal install --only-dependencies --extra-include-dirs=/usr/local/include/zookeeper # if you use brew on Mac OSX
    > cabal install --fsample

How to use
----------

Import System.Prefork in your Main module.

```haskell
import System.Prefork
```

Define data type used for server configuration.

```haskell
data Config = Config {
    cWarpSettings :: Warp.Settings
  }
```

Define workers as a data type that belongs to 'WorkerContext' class.
In this case, the field 'wId' is a ID number for idenfitying single worker process and other fields are
parameters for a worker process.

```haskell
data Worker = Worker {
    wId       :: Int
  , wPort     :: Int
  , wSocketFd :: CInt
  , wHost     :: String
  , wCap      :: Int
  } deriving (Show, Read)

instance WorkerContext Worker where
  rtsOptions w = ["-N" ++ show (wCap w)]
```

Define Eq and Ord instances for Worker. These are required for using 'relaunchSettings'.

```haskell
instance Eq Worker where
  (==) a b = wId a == wId b

instance Ord Worker where
  compare a b = compare (wId a) (wId b)
```

Call 'defaultMain' with 'update' and 'fork' functions in your 'main' function.
'relaunchSettings' is a function that creates comvenient settings for a typical prefork server.
 
```haskell
main :: IO ()
main = do
  option <- cmdArgs cmdLineOptions
  resource <- emptyPreforkResource
  mSoc <- newTVarIO Nothing
  let s = Server mSoc (port option) (workers option)
  defaultMain (relaunchSettings resource (update s) (fork s)) $ \(Worker { wId = i, wSocketFd = fd }) -> do
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
```

'update' function is used for modifying the worker process configuration.
If you want to increase or decrease the number of workers, change worker parameters, and etc,
you can use 'updateWorkerSet' function here.

```haskell
    update :: Server -> PreforkResource Worker -> IO (Maybe Config)
    update s resource = do
      mConfig <- updateConfig s
      updateWorkerSet resource $ flip map [0..(sWorkers s - 1)] $ \i ->
        Worker { wId = i, wPort = (sPort s), wSocketFd = -1, wHost = \"localhost\", wCap = sWorkers s }
      return (mConfig)
```

'fork' function simply creates a worker process with the given parameters.
You should call 'forkWorkerProcess' or 'forkWorkerProcessWithArgs' in this function to invoke a child process as a worker.
In this case, the arguments of 'forkWorkerProcessWithArgs' are just for displaying id number and not used.

```haskell
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
      forkWorkerProcessWithArgs (w') [\"id=\" ++ show (wId w') ]
```

Please see warp.hs in sample/ for further information.

Examples
--------

### Simple

prefork-sample-simple is a very simple example for showing the basic feature of this library.

    > prefork-sample-simple
    Please send SIGHUP to 12345 to relaunch a worker <- parent process
    "Hello. I'm a worker."                           <- child process

Open another terminal and send SIGHUP to the child process.

    > kill -HUP 12345

Then, you will see the message again.

    Please send SIGHUP to 12345 to relaunch a worker <- parent process
    "Hello. I'm a worker."                           <- child process
    "Hello. I'm a worker."                           <- another child process

### Warp

prefork-sample-warp is a more complex and practical example for showing relaunch feature.

    > prefork-sample-warp -p 3000 -w 5

Open another terminal and execute ps command to see the parent and child processes.

    > ps ax
    ...
    17307 s017  S+     0:00.02 .cabal-sandbox/bin/prefork-sample-warp -p 3000 -w 5
    17308 s017  S+     0:00.07 .cabal-sandbox/bin/prefork-sample-warp id=0 +RTS -N5 -RTS
    17309 s017  S+     0:00.04 .cabal-sandbox/bin/prefork-sample-warp id=1 +RTS -N5 -RTS
    17310 s017  S+     0:00.05 .cabal-sandbox/bin/prefork-sample-warp id=2 +RTS -N5 -RTS
    17311 s017  S+     0:00.10 .cabal-sandbox/bin/prefork-sample-warp id=3 +RTS -N5 -RTS
    17312 s017  S+     0:00.04 .cabal-sandbox/bin/prefork-sample-warp id=4 +RTS -N5 -RTS
    ...

And, kill one of them.

    > kill 17310
    > ps ax
    ...
    17307 s017  S+     0:00.03 .cabal-sandbox/bin/prefork-sample-warp -p 3000 -w 5
    17308 s017  S+     0:00.27 .cabal-sandbox/bin/prefork-sample-warp id=0 +RTS -N5 -RTS
    17309 s017  S+     0:00.21 .cabal-sandbox/bin/prefork-sample-warp id=1 +RTS -N5 -RTS
    17311 s017  S+     0:00.51 .cabal-sandbox/bin/prefork-sample-warp id=3 +RTS -N5 -RTS
    17312 s017  S+     0:00.48 .cabal-sandbox/bin/prefork-sample-warp id=4 +RTS -N5 -RTS
    18697 s017  S+     0:00.03 .cabal-sandbox/bin/prefork-sample-warp id=2 +RTS -N5 -RTS
    ...

The process with id number 2 has been relaunched by the parent process.



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
import System.Posix
import System.Exit (exitSuccess)
```

Define data type used for server configuration.

```haskell
data ServerConfig = ServerConfig
```

Define workers as a data type.

```haskell
data Worker = Worker1 String deriving (Show, Read)

instance WorkerContext Worker
```

Call System.Prefork.defaultMain function with settings in your main function.
 
```haskell
main :: IO ()
main = defaultMain defaultSettings {
    psUpdateConfig = updateConfig
  , psUpdateServer = updateServer
  , psOnStart      = \_ -> do
      pid <- getProcessID
      putStrLn $ "Please send SIGHUP to " ++ show pid ++ " to relaunch a worker"
  } $ \so -> case so of
  Worker1 msg -> print msg >> exitSuccess

updateConfig :: IO (Maybe ServerConfig)
updateConfig = do
  return (Just ServerConfig)

updateServer :: ServerConfig -> IO ([ProcessID])
updateServer ServerConfig = do
  pid <- forkWorkerProcess (Worker1 "Hello. I'm a worker.")
  return ([pid])
```

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


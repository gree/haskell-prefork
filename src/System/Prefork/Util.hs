
module System.Prefork.Util (
    sendSignal
  , setSignalHandler
  ) where

import Control.Monad
import Control.Exception
import System.Posix

sendSignal :: Signal -> ProcessID -> IO ()
sendSignal sig cid = signalProcess sig cid `catch` ignore
  where
    ignore :: SomeException -> IO ()
    ignore _ = return ()

setSignalHandler :: Signal -> System.Posix.Handler -> IO ()
setSignalHandler sig func = void $ installHandler sig func Nothing


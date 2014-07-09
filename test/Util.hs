module Util (getDistDir, cabalSandboxDistDir) where

import Data.Word
import Data.List
import Data.Char
import Data.Bits
import Numeric
import System.FilePath
import System.Directory
import Distribution.Cab.Sandbox
import Control.Monad
import Control.Exception


getDistDir :: FilePath -> IO (FilePath)
getDistDir path = do
  mSandboxPath <- bracket (getCurrentDirectory) (setCurrentDirectory) $ \_ -> do
    setCurrentDirectory path
    liftM (fmap takeDirectory) getSandbox
  return $ case mSandboxPath of
    Just sandboxPath -> "dist" </> (cabalSandboxDistDir sandboxPath)
    Nothing -> "dist"

cabalSandboxDistDir :: String -> String
cabalSandboxDistDir path = "dist-sandbox-" ++ showHex (jenkins path) ""

jenkins :: String -> Word32
jenkins str = loop_finish $ foldl' loop 0 str
  where
    loop :: Word32 -> Char -> Word32
    loop hash key_i' = hash'''
      where
        key_i   = toEnum . ord $ key_i'
        hash'   = hash + key_i
        hash''  = hash' + shiftL hash' 10
        hash''' = hash'' `xor` shiftR hash'' 6
    loop_finish :: Word32 -> Word32
    loop_finish hash = hash'''
      where
        hash'   = hash + shiftL hash 3
        hash''  = hash' `xor` shiftR hash' 11
        hash''' = hash'' + shiftL hash'' 15

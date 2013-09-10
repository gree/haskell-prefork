
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | This is a library for servers based on worker process model.
-}
module System.Prefork(
    module System.Prefork.Types
  , module System.Prefork.Main
  ) where

import System.Prefork.Types
import System.Prefork.Main

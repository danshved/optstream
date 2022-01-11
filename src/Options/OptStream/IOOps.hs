{-|
Module      : Options.OptStream.Raw
Copyright   : (c) Dan Shved, 2022
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

This module defines 'IOOps', which is a way to abstract away IO operations
required for "Options.OptStream".
-}
module Options.OptStream.IOOps where

import qualified System.Exit
import qualified System.Environment
import qualified System.IO

-- | 'IOOps' is a typeclass that describes parts of 'IO' used by
-- "Options.OptStream". It is meant to be represented by 'IO' in production and
-- a mock implementation in tests.
class Monad m => IOOps m where
  getArgs :: m [String]
  getProgName :: m String
  putStrLn :: String -> m ()
  die :: String -> m a
  exitSuccess :: m a

instance IOOps IO where
  getArgs = System.Environment.getArgs
  getProgName = System.Environment.getProgName
  putStrLn = System.IO.putStrLn
  die = System.Exit.die
  exitSuccess = System.Exit.exitSuccess


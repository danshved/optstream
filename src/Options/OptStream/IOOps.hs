module Options.OptStream.IOOps where

import qualified System.Exit
import qualified System.Environment
import qualified System.IO

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

-- TODO: Add haddock.

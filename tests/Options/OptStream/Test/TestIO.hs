{-|
Module      : Options.OptStream.Test.TestIO
Copyright   : (c) Dan Shved, 2022
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

Mock implementation of 'IOOps', used to test IO-related parts of
"Options.OptStream".
-}

{-# LANGUAGE DeriveGeneric #-}
module Options.OptStream.Test.TestIO where

import Control.Arrow
import Control.Monad
import GHC.Generics

import Options.OptStream.IOOps

-- | Mock program environment for 'TestIO'.
data TestEnv = TestEnv
 { progName :: String
 , args :: [String]
 }
 deriving (Show, Generic)

data TestResult a
 = TestReturn a
 | TestExitSuccess
 | TestDie !String
 deriving (Eq, Show)

isDie :: TestResult a -> Bool
isDie (TestDie _) = True
isDie _ = False


-- | Replacement of IO for testing. Records standard output and exit outcome
-- and lets one inspect it afterwards.
newtype TestIO a = TestIO { runTestIO :: TestEnv -> (TestResult a, String) }

deepString :: String -> ()
deepString = foldr seq ()

deepResult :: TestResult a -> ()
deepResult (TestDie s) = deepString s `seq` ()
deepResult _ = ()

-- Like 'runTestIO', but forces evaluation of stdout and the error message
-- passed to 'die', if any.
runTestIO' :: TestIO a -> TestEnv -> (TestResult a, String)
runTestIO' ta env = deepResult res `seq` deepString out `seq` (res, out)
  where
    (res, out) = runTestIO ta env

instance Functor TestIO where
  fmap = liftM

instance Applicative TestIO where
  pure = return
  (<*>) = ap

instance Monad TestIO where
  return x = TestIO $ \_ -> (TestReturn x, mempty)

  a >>= f = TestIO $ \env -> case runTestIO a env of
    (TestReturn aVal, out) -> second (out <>) $ runTestIO (f aVal) env
    (TestExitSuccess, out) -> (TestExitSuccess, out)
    (TestDie s, out) -> (TestDie s, out)

instance IOOps TestIO where
  getArgs = TestIO $ \(TestEnv _ as) -> (TestReturn as, mempty)
  getProgName = TestIO $ \(TestEnv s _) -> (TestReturn s, mempty)
  putStrLn s = TestIO $ \_ -> (TestReturn (), s ++ "\n")
  die msg = TestIO $ \_ -> (TestDie msg, mempty)
  exitSuccess = TestIO $ \_ -> (TestExitSuccess, mempty)


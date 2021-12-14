module Main where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Functor
import Options.OptStream

-- Options that can show up to the left of '--'.
data Options = Options
  { bool     :: Bool
  , int      :: Int
  , freeArgs :: [String]
  }

optionsP :: Parser Options
optionsP = Options
  <$> (flag ["-b", "--bool"] "Boolean flag." $> True <|> orElse False)
  <#> (paramRead ["-i", "--int"] "INT" "Integer parameter." <|> orElse 0)
  <#> many (freeArg "LEFT" "Free arguments to the left of --.")

run :: Options -> [String] -> IO ()
run opts args = do
  putStrLn $ "bool       : " ++ show (bool opts)
  putStrLn $ "int        : " ++ show (int opts)
  putStrLn $ "left of -- : " ++ show (freeArgs opts)
  putStrLn $ "right of --: " ++ show args

main = join . parseArgsWithHelp
  $ header "Usage: dashes [options] LEFT... [-- RIGHT...]"
  $ sortTable
  $ run
  <$> beforeDashes optionsP
  <*> many (anyArg "RIGHT" "Arguments to the right of --.")

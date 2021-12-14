module Main where

import Control.Applicative
import Control.Monad
import Options.OptStream

hello :: String -> IO ()
hello name = putStrLn $ "Hello, " ++ name ++ "!"

main :: IO ()
main = join . parseArgs
  $ withHelpIO'
  $ flagHelp ["--help"] "Print this special help message!"
  $ header "Usage: hello [NAME]"
  $ hello <$> (freeArg' "NAME" <|> orElse "James Bond")

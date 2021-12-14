module Main where

import Control.Applicative
import Control.Monad
import Options.OptStream

hello :: String -> IO ()
hello name = putStrLn $ "Hello, " ++ name ++ "!"

main :: IO ()
main = join . parseArgsWithHelp
  $ withVersionIO "Hello, version 1.0"
  $ header "Usage: hello [NAME]"
  $ footer "Example: hello \'Sherlock Holmes\'"
  $ hello
  <$> (freeArg "NAME" "Your name (optional)." <|> orElse "James Bond")

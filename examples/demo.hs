-- Introductory example.
module Main where

import Control.Applicative
import Data.Functor
import Options.OptStream

data Options = Options
  { strParam   :: String
  , intParam   :: Int
  , boolFlag   :: Bool
  , positional :: String
  }
  deriving Show

optionsP :: Parser Options
optionsP = Options
  <$> (param ["-s", "--string"] "STR" "String parameter." <|> orElse "")
  <#> (paramRead ["-i", "--int"] "INT" "Integer parameter." <|> orElse 0)
  <#> (flag ["-b", "--bool"] "Boolean flag." $> True <|> orElse False)
  <#> (freeArg "ARG" "Positional argument.")

main = do
  opts <- parseArgsWithHelp
    $ header "Usage: demo [options] ARG"
    $ footer "Example: demo -b --int=42 foo"
    $ optionsP

  print opts

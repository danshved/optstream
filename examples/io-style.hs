module Main where

import Control.Applicative
import Control.Monad
import Options.OptStream

copy :: String -> String -> IO ()
copy src dst = do
  contents <- readFile src
  writeFile dst contents

main :: IO ()
main = join . parseArgsWithHelp
  $ header "Usage: copy -i FILE -o FILE"
  $ copy
  <$> param ["-i", "--input"] "FILE" "Input file."
  <#> param ["-o", "--output"] "FILE" "Output file."

module Main where

import Options.OptStream

main :: IO ()
main = do
  (src, dst) <- parseArgsWithHelp
    $ header "Usage: copy [options]"
    $ footer "Example: copy -i input.txt -o output.txt"
    $ (,)
    <$> param ["-i", "--input"] "FILE" "Input file."
    <#> param ["-o", "--output"] "FILE" "Output file."

  contents <- readFile src
  writeFile dst contents

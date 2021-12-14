module Main where

import Control.Applicative hiding (optional)
import Control.Monad
import Options.OptStream

send :: String -> String -> IO ()
send src dst = putStrLn $ "Would send " ++ show dst ++ " to " ++ src ++ "."

fetch :: Maybe Int -> IO ()
fetch Nothing = putStrLn $ "Would fetch all emails."
fetch (Just n) = putStrLn $ "Would fetch at most " ++ show n ++ " emails."

main :: IO ()
main = join . parseArgsWithHelp
  $   header "Usage: email (send | fetch) [options]"

  $   match "send" *> ( withSubHelpIO
        $ header "Usage: email send --to=EMAIL BODY"
        $ footer "Example: email send --to=foo@bar.com \'Hello, world!\'"
        $ send
        <$> param ["--to"] "EMAIL" "Recipient."
        <#> freeArg "BODY" "Email body."
      )

  <|> match "fetch" *> ( withSubHelpIO
        $ header "Usage: email fetch [--limit=N]"
        $ footer "Example: email fetch --limit=10"
        $ fetch
        <$> optional (paramRead ["--limit"] "N" "Limit email count.")
      )

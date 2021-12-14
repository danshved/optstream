module Main where

import Control.Applicative hiding (optional)
import Control.Monad
import Options.OptStream.Help
import Options.OptStream

data Command
  = Send String String
    -- ^ Send email to given recipient with given content.
  | Fetch (Maybe Int)
    -- ^ Fetch emails, with optional count limit.
  deriving Show

commandP :: Parser (Either Help Command)
commandP = join <$> ( withHelp
  $   header "Usage: email (send | fetch) [options]"

  $   match "send" *> ( withSubHelp
        $ header "Usage: email send --to=EMAIL BODY"
        $ footer "Example: email send --to=foo@bar.com \'Hello, world!\'"
        $ Send
        <$> param ["--to"] "EMAIL" "Recipient."
        <#> freeArg "BODY" "Email body."
      )

  <|> match "fetch" *> ( withSubHelp
        $ header "Usage: email fetch [--limit=N]"
        $ footer "Example: email fetch --limit=10"
        $ Fetch
        <$> optional (paramRead ["--limit"] "N" "Limit email count.")
      )
  )


main :: IO ()
main = return ()

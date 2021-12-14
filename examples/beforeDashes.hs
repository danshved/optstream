module Main where

import Control.Applicative hiding (many)
import Data.Char
import Data.Functor
import Data.List
import Options.OptStream

transformP :: Parser (Char -> Char)
transformP
  =   flag' ["-u", "--uppercase"] $> toUpper
  <|> flag' ["-l", "--lowercase"] $> toLower
  <|> orElse id

main :: IO ()
main = do
  (transform, args) <- parseArgs $ (,)
    <$> beforeDashes transformP
    <#> many (anyArg' "WORD")

  putStrLn . map transform . concat . intersperse " " $ args

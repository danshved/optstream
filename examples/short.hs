module Main where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.Functor
import Options.OptStream

letter :: Parser Char
letter = short "LETTER" $ \c -> guard (isLetter c) $> c

digit :: Parser Char
digit = short "DIGIT" $ \c -> guard (isDigit c) $> c

main = return ()

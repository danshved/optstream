{-|
Module      : Options.OptStream
Copyright   : (c) Dan Shved, 2021
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

This module contains 'Parser', the twice-applicative type constructor for
command line parsers. A basic example:

@
module Main where

import Control.Applicative
import Data.Functor
import "Options.OptStream"

data Options = Options
  { strParam   :: String
  , intParam   :: Int
  , boolFlag   :: Bool
  , positional :: String
  }
  deriving Show

optionsP :: 'Parser' Options
optionsP = Options
  '<$>' ('param' ["-s", "--string"] \"STR\" "String parameter." '<|>' 'orElse' "")
  '<#>' ('paramRead' ["-i", "--int"] \"INT\" "Integer parameter." '<|>' 'orElse' 0)
  '<#>' ('flag' ["-b", "--bool"] "Boolean flag." '$>' True '<|>' 'orElse' False)
  '<#>' ('freeArg' "\ARG\" "Positional argument.")

main = do
  opts <- 'parseArgsWithHelp'
    $ 'header' "Usage: demo [options] ARG"
    $ 'footer' "Example: demo -b --int=42 foo"
    $ optionsP

  putStrLn $ "String parameter   : " ++ show (strParam opts)
  putStrLn $ "Integer parameter  : " ++ show (intParam opts)
  putStrLn $ "Boolean flag       : " ++ show (boolFlag opts)
  putStrLn $ "Positional argument: " ++ show (positional opts)
@

Note that in the code above:

  * We build a parser from /atomic/ parsers. See 'flag', 'param', 'freeArg'.

  * We combine them together using /parallel application/ '<#>', which allows
  parsing the options in any order.

  * We make options optional by using the 'Alternative' operator '<|>' together
  with 'orElse'.

  * We run the parser using 'parseArgsWithHelp', which takes care of handling
  errors and printing @--help@.

==== __Demo outputs:__

>>> ./demo -s foo -i 42 -b bar
String parameter   : "foo"
Integer parameter  : 42
Boolean flag       : True
Positional argument: "bar"

>>> ./demo foo
String parameter   : ""
Integer parameter  : 0
Boolean flag       : False
Positional argument: "foo"

>>> ./demo --help
Usage: demo [options] ARG
<BLANKLINE>
  -s, --string=STR  String parameter.
  -i, --int=INT     Integer parameter.
  -b, --bool        Boolean flag.
  ARG               Positional argument.
      --help        Show this help message and exit.
<BLANKLINE>
Example: demo -b --int=42 foo

-}
module Options.OptStream
  ( -- * Parsers
    Parser
  , runParser
  , runParserIO
  , parseArgs
  , parseArgsWithHelp

    -- * Atomic parsers
  , OptionForm
  , isLegalOptionForm
    -- ** Flags
  , flag
  , flag'
  , flagSep
  , flagSep'
    -- ** Parameters
  , param
  , param'
  , paramRead
  , paramRead'
  , paramChar
  , paramChar'
    -- ** Free arguments
  , freeArg
  , freeArg'
  , freeArgRead
  , freeArgRead'
  , freeArgChar
  , freeArgChar'
  , anyArg
  , anyArg'
    -- ** Multi-parameters
  , multiParam
  , multiParam'
  , Follower
  , next
  , nextRead
  , nextChar
  , nextMetavar

    -- * Re-exported modules
  , module Options.OptStream.Classes

    -- * Utilities
  , withHelp
  , withHelp'
  , withSubHelp
  , withSubHelp'
  , withVersion
  , withVersion'
  , beforeDashes
    -- ** IO-style parsers
    -- $io-style-parsers
  , withHelpIO
  , withHelpIO'
  , withSubHelpIO
  , withSubHelpIO'
  , withVersionIO
  , withVersionIO'

    -- * Low-level parsers
  , block
  , short
  , match
  , matchAndFollow
  , matchShort
  , quiet
  , eject

  -- * Manipulating help
  , header
  , footer
  , flagHelp
  , paramHelp
  , freeArgHelp
  , multiParamHelp
  , clearHelp
  , clearHeader
  , clearFooter
  , clearTable
  , sortTable
  , getHelp
  , setHelp
  , modifyHelp
  , getFollowerHelp
  , setFollowerHelp
  , modifyFollowerHelp

    -- * Raw parsers
    -- $raw
  , toRaw
  , fromRaw
  , toRawFollower
  , fromRawFollower

    -- * Errors
  , ParserError
  , formatParserError
  )
where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Functor
import Data.List hiding (last)
import Data.Maybe
import Data.Monoid
import System.Environment
import System.Exit

import Options.OptStream.Classes
import Options.OptStream.Help
import Options.OptStream.Internal
import Options.OptStream.Raw
  ( RawParser
  , RawFollower
  , ParserError
  , formatParserError
  )
import qualified Options.OptStream.Raw as R

-- $raw
-- Command line parsers are twice applicative thanks to two application
-- operators: '<*>' and '<#>'. In reality they are also monadic, i.e. they have
-- monadic bind '>>='. However, 'Parser' hides this monadic structure.  The
-- reason for that is that 'Parser' produces 'Help', and there's no good way to
-- generate help for @a >>= f@, because it is unknown what @f@ will return at
-- runtime.
--
-- Therefore, 'Parser' is 'Applicative' but not a 'Monad'. However, the monadic
-- structure can still be accessed using 'RawParser'. 'RawParser' doesn't
-- generate help but does offer monadic bind. If you need it, you can build a
-- 'RawParser' and then add 'Help' manually using e.g.  'setHelp', or not add
-- any help at all.
--
-- We provide functions to convert between 'Parser' and 'RawParser', and
-- likewise for 'Follower' and 'RawFollower'. When converting from "rich" to
-- "raw", help information is lost. When converting back, empty or default help
-- is generated.


-- * Follower applicative

-- | A 'Follower' consumes a (prefix of a) stream of command line arguments and
-- produces a value of type @a@. Unlike a 'Parser', a 'Follower' cannot decide
-- to skip an argument based on its value. Once the 'Follower' has read an
-- argument, the argument is consumed, and the 'Follower' can decide to either
-- stop and produce a result (an @a@), or to read another argument.
--
-- You work with followers in the following way:
--
-- * Start with primitive followers ('next' and related wrappers).
-- * Combine them using the 'Applicative' instance ('<*>' etc.).
-- * Pass a 'Follower' to 'multiParam', or return your 'Follower' to 'block' if
--   you're doing low-level things.
data Follower a = Follower
  { toRawFollower   :: RawFollower a
    -- ^ Retrieves the actual 'RawFollower' object backing the given
    -- 'Follower'.
  , getFollowerHelp :: String
    -- ^ Retrieves the help string stored in a 'Follower'. This string is used
    -- in the help generated by 'multiParam'.
  }

-- | Converts a 'RawFollower' into a 'Follower'. The 'Follower' will have
-- exactly the same behavior as the 'RawFollower', and it will get a default
-- help string (either @""@ or @"..."@ depending on whether the follower wants
-- any input). You can replace the default help string with your own using
-- 'setFollowerHelp'.
fromRawFollower :: RawFollower a -> Follower a
fromRawFollower raw = Follower raw h where
  h = case R.nextMetavar raw of
    Nothing -> ""
    Just _ -> "..."

-- | Changes the help string stored in a 'Follower'.
setFollowerHelp :: String -> Follower a -> Follower a
setFollowerHelp h (Follower raw _) = Follower raw h

-- | Modifies the help string stored in a 'Follower' using a given function.
modifyFollowerHelp :: (String -> String) -> Follower a -> Follower a
modifyFollowerHelp f (Follower raw h) = Follower raw $ f h

liftF0 :: RawFollower a -> Follower a
liftF0 = fromRawFollower

liftF1 :: (RawFollower a -> RawFollower b) -> (Follower a -> Follower b)
liftF1 f (Follower raw h) = Follower (f raw) h

instance Functor Follower where
  fmap = liftF1 . fmap

instance FunctorFail Follower where
  fmapOrFail = liftF1 . fmapOrFail

instance Applicative Follower where
  pure = liftF0 . pure
  Follower raw h <*> Follower raw' h' =
    Follower (raw <*> raw') (catSpace h h') where
      catSpace "" x = x
      catSpace x "" = x
      catSpace x y = x ++ " " ++ y

instance ApplicativeFail Follower where
  failA = liftF0 . failA

-- | A 'Follower' that consumes one argument and returns it verbatim.
next :: String
        -- ^ Metavariable for help and error messages.
     -> Follower String
next = addNextHelp R.next

-- | Returns the metavariable corresponding to the next argument that the
-- 'Follower' wants to consume. 'Nothing' if the follower doesn't want any more
-- input. The following identities hold:
--
-- > nextMetavar (next x) = Just x
-- > nextMetavar (pure a) = Nothing
nextMetavar :: Follower a -> Maybe String
nextMetavar = R.nextMetavar . toRawFollower

-- * Parser

-- | A 'Parser' processes (part of) a stream of command line arguments and
-- produces an output value of type @a@. It also contains information necessary
-- to generate help.
--
-- The general steps for working with parsers are:
--
--   * Create atomic parsers for your options with functions like 'flag',
--   'param', 'freeArg' etc., see below.
--
--   * Use combinators '<$>', '<#>', '<|>', '<*>' and others to produce one
--   single @'Parser' a@. You can find some useful combinators in classes
--   'SubstreamParser', 'FunctorFail', and 'ApplicativeFail'.
--
--   * Run the parser with 'runParser' or one of the convenience wrappers, such
--   as 'parseArgsWithHelp'.
data Parser a = Parser
  { toRaw   :: RawParser a
    -- ^ Retrieves the actual 'RawParser' object backing the given 'Parser'.
  , getHelp :: Help
    -- ^ Retrieves the 'Help' object stored in a given 'Parser'.
  }

-- | 'runParser' is the most basic way of running a parser. Returns 'Right' in
-- case of success and 'Left' in case of failure.
--
-- >>> runParser (param' ["--foo"] "FOO") ["--foo=bar"]
-- Right "bar"
--
-- >>> runParser (param' ["--foo"] "FOO") []
-- Left (MissingArg CtxEnd ["--foo"])
runParser :: Parser a -> [String] -> Either ParserError a
runParser = R.runParser . toRaw

-- | Converts a 'RawParser' into a 'Parser'. The 'Parser' has the exact same
-- parsing behavior as the 'RawParser', and an empty 'Help' attached to it. You
-- can attach your own 'Help' to the 'Parser' using 'setHelp' or a number of
-- other helper functions, e.g. 'header' and 'footer'.
fromRaw :: RawParser a -> Parser a
fromRaw pa = Parser pa mempty

lift0 :: RawParser a -> Parser a
lift0 = fromRaw

lift1 :: (RawParser a -> RawParser b) -> (Parser a -> Parser b)
lift1 f (Parser raw h) = Parser (f raw) h

lift2 :: (RawParser a -> RawParser b -> RawParser c)
      -> (Parser a -> Parser b -> Parser c)
lift2 f (Parser raw h) (Parser raw' h') = Parser (f raw raw') (h <> h')


-- ** Instances

instance Functor Parser where
  fmap = lift1 . fmap

instance FunctorFail Parser where
  fmapOrFail = lift1 . fmapOrFail

instance Applicative Parser where
  pure = lift0 . pure
  (<*>) = lift2 (<*>)

instance ApplicativeFail Parser where
  failA = lift0 . failA

instance Alternative Parser where
  empty = lift0 empty
  (<|>) = lift2 (<|>)

instance SubstreamParser Parser where
  eof = lift0 . eof
  (<#>) = lift2 (<#>)
  (<-#>) = lift2 (<-#>)
  (<#->) = lift2 (<#->)
  (<-|>) = lift2 (<-|>)
  (<|->) = lift2 (<|->)
  many = lift1 many
  some = lift1 some
  between min max = lift1 $ between min max
  perm xs = Parser (perm $ map toRaw xs) (foldMap getHelp xs)


-- ** Primitive parsers

-- | The most general atomic parser. All the other atomic parsers in this
-- library are built on top of 'block' (and sometimes 'short').
--
-- 'block' accepts a function that, given a command line argument, decides what
-- to do with it. If the function returns 'Nothing', the parser will /skip/ the
-- argument. If this happens, the parser remains in its original state, as if
-- the argument was never seen. The argument can then be consumed by another
-- 'Parser' running in parallel with this one (via e.g. '<#>' or '<|>').
--
-- Alternatively, the function can return a 'Just' value with a 'Follower'. In
-- this case the 'Parser' is considered to have /consumed/ the argument. After
-- that the 'Follower' seizes control and has the option to consume more
-- arguments immediately after the current one. Finally, when the 'Follower'
-- releases the stream and produces a value of type @a@, that value becomes the
-- result of the parser.
block :: String
         -- ^ Block name for "missing argument" error messages. Arbitrary
         -- string.
      -> (String -> Maybe (Follower a))
         -- ^ A function that decides whether to skip or consume a command line
         -- argument.
      -> Parser a
         -- ^ A 'Parser' that consumes one consecutive block of command line
         -- arguments.
block name = lift0 . R.block name . (fmap toRawFollower .)

-- | General atomic parser for short flags with bundling.
--
-- 'short' accepts a function that, given a 'Char' representing a short flag,
-- decides what to do with it. The options are: /skip/ the flag (by returning
-- 'Nothing'), or /consume/ the flag and return a value of type @a@ (by
-- returning @'Just' a@).
--
-- ==== __Example:__
--
-- > letter :: Parser Char
-- > letter = short "LETTER" $ \c -> guard (isLetter c) $> c
-- >
-- > digit :: Parser Char
-- > digit = short "DIGIT" $ \c -> guard (isDigit c) $> c
--
-- >>> let p = (,) <$> many letter <#> many digit
-- >>> runParserIO p ["-a", "-1", "-b2c3"]
-- ("abc","123")
short :: String
         -- ^ Short flag name for "missing argument" error messages. Arbitrary
         -- string.
      -> (Char -> Maybe a)
         -- ^ A function that decides whether to skip or consume a short flag.
      -> Parser a
         -- ^ A 'Parser' that consumes one short flag.
short name = lift0 . R.short name

-- | Suppresses "missing argument" suggestions from the 'Parser'. This is used
-- in the implementation of 'withHelp' and 'withVersion', so that @--help@ and
-- @--version@, which are always valid arguments, don't show up in error
-- messages.
--
-- Note that 'quiet' only works until the parser consumes some input. Once the
-- parser has consumed an argument, it is in a new state and no longer quiet.
--
-- ==== __Example:__
--
-- >>> let p = flag' ["-a"] <|> quiet (flag' ["-b"]) <|> flag' ["-c"]
-- >>> runParserIO p []
-- <interactive>: missing command line argument: -a | -c
quiet :: Parser a -> Parser a
quiet = lift1 R.quiet

-- | Helper: run a 'Parser' with an option to "eject".
--
-- Parser @a@ runs normally, but parser @b@ gets to look at every argument that
-- parser @a@ has skipped (even after parser @a@ has finished). If EOF is
-- reached and parser @b@ never consumes anything, then @a@'s result is
-- returned normally as a 'Right' value.  However, if parser @b@ consumes an
-- argument, parser @a@ is killed ("ejected" from), all its state discarded.
-- Parser @b@ then runs until the end and its result is returned in a 'Left'
-- value. Any arguments left unread after @b@ has finished are also discarded.
--
-- This is used in the implementation of @--help@ and @--version@. You can use
-- it to make similar-behaving flags.
eject :: Parser a
         -- ^ An existing parser.
      -> Parser b
         -- ^ A parser that may trigger an ejection.
      -> Parser (Either b a)
eject = lift2 R.eject


-- ** Matchers

-- | Consumes and returns the exact given string. Skips any other argument.
match :: String -> Parser String
match = lift0 . R.match

-- | Consumes a block of command line arguments starting with the exact given
-- string. Once the string is consumed, the rest of the block is consumed by
-- the given 'Follower'.
matchAndFollow :: String
                  -- ^ Command line argument that starts a block.
               -> Follower a
                  -- ^ A follower that consumes the rest of the block.
               -> Parser a
matchAndFollow s = lift0 . R.matchAndFollow s . toRawFollower

-- | Consumes and returns the exact given short flag, skips everything else.
matchShort :: Char
              -- ^ A short flag, e.g. @\'x\'@ will match @-x@ or an occurence
              -- of @\'x\'@ in a bundle of short flags like @-xyz@.
           -> Parser Char
matchShort = lift0 . R.matchShort


-- ** High level parsers with built-in help


-- *** Flag

-- | A /flag/ is a simple option with no arguments. It is simply there or not
-- there. For example, @sort@ from GNU coreutils has a flag @-r@, @--reverse@
-- to sort in reverse order.
--
-- The first argument to 'flag' is for all the forms of the flag, both short
-- and long. You can pass as many forms as you like. They will all match, but
-- only the first one of each kind (short and long), if any, will appear in the
-- generated help.
--
-- An empty list or a list containing illegal forms will result in an 'error'
-- (see 'OptionForm').
--
-- Since a flag doesn't carry any information except for its own presence, the
-- returned value is @'Parser' ()@. If you want to turn it into a 'Bool' that
-- is 'False' by default and turns to 'True' when the flag is present, you can
-- do that using the @'$>' '<|>' 'orElse'@ idiom:
--
-- >>> let f = flag ["-v", "--verbose"] "Verbose output." $> True <|> orElse False
-- >>> runParserIO f []
-- False
-- >>> runParserIO f ["-v"]
-- True
--
-- Short forms of flags can be bundled together, e.g. @-ab@ will work the same
-- as @-a -b@.  If you don't want bundling, use 'flagSep' instead.
--
-- ==== __Example (bundling):__
--
-- >>> let foo = flag ["-f"] "Foo" $> "foo" <|> orElse "no foo"
-- >>> let bar = flag ["-b"] "Bar" $> "bar" <|> orElse "no bar"
-- >>> let foobar = (,) <$> foo <#> bar
--
-- >>> runParserIO foobar ["-f"]
-- ("foo", "no bar")
--
-- >>> runParserIO foobar ["-b"]
-- ("no foo", "bar")
--
-- >>> runParserIO foobar ["-f", "-b"]
-- ("foo", "bar")
--
-- >>> runParserIO foobar ["-fb"]
-- ("foo", "bar")
--
-- >>> runParserIO foobar ["-bf"]
-- ("foo", "bar")
flag :: [OptionForm]
        -- ^ Flag forms, e.g. @["-f", "--foo"]@.
     -> String
        -- ^ Description for help.
     -> Parser ()
        -- ^ A parser that succeeds upon consuming the flag.
flag ss desc = flagHelp ss desc $ flag' ss


-- | Like 'flag' but doesn't generate any help.
flag' :: [OptionForm]
         -- ^ Flag forms, e.g. @["-f", "--foo"]@.
      -> Parser ()
         -- ^ A parser that succeeds upon consuming the flag.
flag' = lift0 . R.flag'


-- | Like 'flag' but doesn't support bundling. A 'flagSep' will only work
-- separately, it will not bundle with other flags, even if they are defined
-- with 'flag'.
--
-- ==== __Example (no bundling):__
--
-- >>> let foo = flag ["-f"] "Foo" $> "foo" <|> orElse "no foo"
-- >>> let bar = flagSep ["-b"] "Bar" $> "bar" <|> orElse "no bar"
-- >>> let foobar = (,) <$> foo <#> bar
--
-- >>> runParserIO foobar ["-f", "-b"]
-- ("foo", "bar")
--
-- >>> runParserIO foobar ["-fb"]
-- <interactive>: unexpected character 'b' in command line argument "-fb"
flagSep :: [OptionForm]
           -- ^ Flag forms, e.g. @["-f", "--foo"]@.
        -> String
           -- ^ Description for help.
        -> Parser ()
           -- ^ A parser that succeeds upon consuming the flag.
flagSep ss desc = flagHelp ss desc $ flagSep' ss


-- | Like 'flagSep' but doesn't generate any help.
flagSep' :: [OptionForm]
            -- ^ Flag forms, e.g. @["-f", "--foo"]@.
         -> Parser ()
            -- ^ A parser that succeeds upon consuming the flag.
flagSep' = lift0 . R.flagSep'


-- *** Param

addParamHelp :: ([OptionForm] -> String -> Parser a)
             -> ([OptionForm] -> String -> String -> Parser a)
addParamHelp func opts metavar desc =
  paramHelp opts metavar desc $ func opts metavar

-- | A /parameter/ is an option that has one string argument, e.g.
-- @--input=FILENAME@ or @-n NAME@.
--
-- The first argument to 'param' should list all the forms of the parameter,
-- both short and long. For every short form @-f@ the parser will accept:
--
--  * @-f VALUE@ (two separate arguments). @VALUE@ can be anything, including
--    an empty string.
--
--  * @-fVALUE@ (single argument). In this case @VALUE@ must be a non-empty
--    string, as @-f@ alone would be interpreted as the begining of @-f VALUE@.
--
-- For every long form @--foo@ the parser will accept:
--
--  * @--foo VALUE@ (two separate arguments). @VALUE@ can be anything,
--    including an empty string.
--
--  * @--foo=VALUE@ (single argument). Again, @VALUE@ can be anything,
--    including an empty string.
--
-- You can specify zero or more short forms and zero or more long forms.  There
-- must be at least one form total, otherwise the function will fail with
-- 'error'. If you specify more than one form of a kind (short or long), all
-- the forms will be matched during parsing, but only the first one of each
-- kind will appear in the generated help.
--
-- A 'param' is mandatory. If you want to make it optional, use @'<|>'
-- 'orElse'@.
--
-- ==== __Example (mandatory parameter):__
--
-- >>> let p = param ["-i", "--input"] "FILENAME" "Input filename."
-- >>> runParserIO p ["-i", "foo.txt"]
-- "foo.txt"
--
-- >>> runParserIO p ["--input=bar.txt"]
-- "bar.txt"
--
-- >>> runParserIO p ["--input="]
-- ""
--
-- >>> runParserIO p ["--input"]
-- <interactive>: missing command line argument after "--input": FILENAME
--
-- >>> runParserIO p []
-- <interactive>: missing command line argument: --input | -i
--
-- ==== __Example (optional parameter):__
--
-- >>> let p = param ["-n"] "NAME" "Your name. Default: James Bond." <|> orElse "James Bond"
-- >>> runParserIO p ["-n", "Sherlock Holmes"]
-- "Sherlock Holmes"
--
-- >>> runParserIO p []
-- "James Bond"
param :: [OptionForm]
         -- ^ All parameter forms, e.g. @["-n", "--name"]@.
      -> String
         -- ^ Metavariable for help and error messages. Can be any 'String'.
      -> String
         -- ^ Description for help.
      -> Parser String
         -- ^ A parser that returns the parameter value.
param = addParamHelp param'


-- | Like 'param' but doesn't generate help.
param' :: [OptionForm]
          -- ^ All parameter forms, e.g. @["-n", "--name"]@.
       -> String
          -- ^ Metavariable for error messages.
       -> Parser String
          -- ^ A parser that returns the parameter value.
param' opts metavar = lift0 $ R.param' opts metavar


-- | Like 'param' but parses the parameter value down to a type @'Read' a =>
-- a@. Can be used e.g. for 'Int' and 'Float' params.
--
-- >>> let p = paramRead ["-n", "--number"] "INT" "An integer parameter." :: Parser Int
-- >>> runParserIO p ["--number=42"]
-- 42
--
-- >>> runParserIO p ["--number=fourty_two"]
-- <interactive>: command line error at "--number=fourty_two": Prelude.read: no parse
paramRead :: Read a
          => [OptionForm]
             -- ^ All parameter forms, e.g. @["-n", "--number"]@.
          -> String
             -- ^ Metavariable for help and error messages. Can be any
             -- 'String'.
          -> String
             -- ^ Description for help.
          -> Parser a
             -- ^ A parser that returns the parsed parameter value.
paramRead = addParamHelp paramRead'


-- | Like 'paramRead' but doesn't generate help.
paramRead' :: Read a
           => [OptionForm]
              -- ^ All parameter forms, e.g. @["-n", "--number"]@.
           -> String
              -- ^ Metavariable for error messages.
           -> Parser a
              -- ^ A parser that returns the parsed parameter value.
paramRead' opts metavar = lift0 $ R.paramRead' opts metavar


-- | Like 'param' but parses the parameter value down to a 'Char'. Fails if
-- the value is anything else than one character long.
--
-- >>> let p = paramChar ["-s"] "CHAR" "Separator character."
-- >>> runParserIO p ["-s|"]
-- '|'
--
-- >>> runParserIO p ["-s\n"]
-- '\n'
--
-- >>> runParserIO p ["-sabc"]
-- <interactive>: command line error at "-sabc": expected one character, got 3
paramChar :: [OptionForm]
             -- ^ All parameter forms, e.g. @["-s", "--separator"]@.
          -> String
             -- ^ Metavariable for help and error messages. Can be any
             -- 'String'.
          -> String
             -- ^ Description for help.
          -> Parser Char
             -- ^ A parser that returns the parsed parameter value.
paramChar = addParamHelp paramChar'


-- | Like 'paramChar' but doesn't generate help.
paramChar' :: [OptionForm]
              -- ^ All parameter forms, e.g. @["-s", "--separator"]@.
           -> String
              -- ^ Metavariable for error messages.
           -> Parser Char
              -- ^ A parser that returns the parsed parameter value.
paramChar' opts metavar = lift0 $ R.paramChar' opts metavar


-- *** Free arguments

addFreeArgHelp :: (String -> Parser a) -> (String -> String -> Parser a)
addFreeArgHelp func' metavar desc = freeArgHelp metavar desc $ func' metavar

-- | Matches any /free argument/, i.e. any argument that doesn't start with
-- @-@.  Returns this argument verbatim as a string.
--
-- Like all the other atomic parsers in this module, 'freeArg' is mandatory. It
-- can be made optional with @'<|>' 'orElse'@.
--
-- ==== __Example (mandatory argument):__
--
-- >>> let p = freeArg "FILENAME" "Input file."
-- >>> runParserIO p ["input.txt"]
--
-- "input.txt"
-- >>> runParserIO p [""]
-- ""
--
-- >>> runParserIO p ["--foo"]
-- <interactive>: unexpected command line argument "--foo"
--
-- >>> runParserIO p []
-- <interactive>: missing command line argument: FILENAME
--
-- ==== __Example (optional argument):__
--
-- >>> let p = freeArg "FILENAME" "Output file. Default: a.out." <|> orElse "a.out"
-- >>> runParserIO p ["./binary"]
-- "./binary"
--
-- >>> runParserIO p []
-- "a.out"
freeArg :: String
           -- ^ Metavariable for help and error messages.
        -> String
           -- ^ Description for help.
        -> Parser String
           -- ^ Parser that consumes and returns the first free argument it
           -- sees.
freeArg = addFreeArgHelp freeArg'


-- | Like 'freeArg' but doesn't generate help.
freeArg' :: String
            -- ^ Metavariable for error messages (arbitrary string).
         -> Parser String
            -- ^ Parser that consumes and returns the first free argument it
            -- sees.
freeArg' = lift0 . R.freeArg'


-- | Like 'freeArg' but parses the argument down to a @'Read' a => a@. Can be
-- used to parse e.g. integers and floating point values.
--
-- >>> let p = freeArgRead "NUM" "A floating point argument." :: Parser Float
-- >>> runParserIO p ["2.718"]
-- 2.718
--
-- >>> runParserIO p ["foo"]
-- <interactive>: command line error at "foo": Prelude.read: no parse
freeArgRead :: Read a
            => String
               -- ^ Metavariable for help and error messages.
            -> String
               -- ^ Description for help.
            -> Parser a
               -- ^ Parser that consumes the first free argument it sees and
               -- parses it down to type @a@.
freeArgRead = addFreeArgHelp freeArgRead'


-- | Like 'freeArgRead' but doesn't generate help.
freeArgRead' :: Read a
             => String
                -- ^ Metavariable for error messages (arbitrary string).
             -> Parser a
                -- ^ Parser that consumes the first free argument it sees and
                -- parses it down to type @a@.
freeArgRead' = lift0 . R.freeArgRead'


-- | Like 'freeArg' but parses the argument down to a 'Char'. Note that a free
-- argument cannot begin with @-@, so the parser will never return @\'-\'@.
--
-- >>> let p = freeArgChar "C" "Any character except \'-\'."
-- >>> runParserIO p ["x"]
-- 'x'
--
-- >>> runParserIO p ["-"]
-- <interactive>: unexpected command line argument "-"
--
-- >>> runParserIO p [""]
-- <interactive>: command line error at "": expected one character, got zero
freeArgChar :: String
               -- ^ Metavariable for help and error messages.
            -> String
               -- ^ Description for help.
            -> Parser Char
               -- ^ Parser that consumes the first free argument it sees and
               -- parses it down to a 'Char'.
freeArgChar = addFreeArgHelp freeArgChar'


-- | Like 'freeArgChar' but doesn't generate help.
freeArgChar' :: String
                -- ^ Metavariable for error messages.
             -> Parser Char
                -- ^ Parser that consumes the first free argument it sees and
                -- parses it down to a 'Char'.
freeArgChar' = lift0 . R.freeArgChar'

-- | Consumes and returns /any/ command line argument. Unlike 'freeArg' this
-- parser will also consume arguments starting with @-@, so the following
-- holds:
--
-- > runParser (many (anyArg metavar desc)) xs == Right xs
--
-- In most cases you should prefer 'freeArg'. However, 'anyArg' can be useful
-- in certain situations, for example if you want to collect all arguments
-- after @--@ (see 'beforeDashes').
anyArg :: String
          -- ^ Metavariable for help and error messages.
       -> String
          -- ^ Description for help.
       -> Parser String
          -- ^ Parser that consumes and returns the first argument it sees.
anyArg = addFreeArgHelp anyArg'

-- | Like 'anyArg' but doesn't generate help.
anyArg' :: String
           -- ^ Metavariable for error messages.
        -> Parser String
           -- ^ Parser that consumes and returns the first argument it sees.
anyArg' = lift0 . R.anyArg'


-- *** Multi-parameters

-- | A /multi-parameter/ is an option that takes an arbitrary number of
-- arguments, e.g. @--person NAME AGE@. 'multiParam' lets you parse such
-- options by providing the option form (in this case @--person@), and a
-- special 'Follower' object that reads zero or more arguments that follow (in
-- this case @NAME@ and @AGE@) using 'next'.
--
-- ==== __Example:__
--
-- > data Person = Person
-- >  { name :: String
-- >  , age  :: Int
-- >  }
-- >  deriving Show
-- >
-- > personP :: Parser Person
-- > personP = multiParam
-- >   ["-p", "--person"]
-- >   (Person <$> next "NAME" <*> nextRead "AGE")
-- >   "A person's name and age."
--
-- >>> runParserIO personP ["--person", "John", "20"]
-- Person {name = "John", age = 20}
--
-- >>> runParserIO personP ["--person"]
-- <interactive>: missing command line argument after "--person": NAME
--
-- >>> runParserIO personP ["--person", "John"]
-- <interactive>: missing command line argument after "--person" "John": AGE
multiParam :: [OptionForm]
              -- ^ All multi-parameter forms, e.g. @["-p", "--person"]@.
           -> Follower a
              -- ^ How to process the following arguments.
           -> String
              -- ^ Description for help.
           -> Parser a
              -- ^ A parser that consumes the option form and the following
              -- arguments.
multiParam opts ra desc =
  multiParamHelp opts (getFollowerHelp ra) desc $ multiParam' opts ra

-- | Like 'multiParam' but doesn't generate help.
multiParam' :: [OptionForm]
              -- ^ All multi-parameter forms, e.g. @["-p", "--person"]@.
            -> Follower a
              -- ^ How to process the following arguments.
            -> Parser a
              -- ^ A parser that consumes the option form and the following
              -- arguments.
multiParam' opts ra = lift0 $ R.multiParam' opts (toRawFollower ra)

addNextHelp :: (String -> RawFollower a) -> (String -> Follower a)
addNextHelp func metavar = Follower (func metavar) metavar

-- | Like 'next' but parses the argument down to a @'Read' a => a@. Can be used
-- for parsing integers and floating point numbers.
--
-- Fails if the next argument cannot be parsed as a value of type @a@.
--
-- >>> let p = multiParam ["-n"] (nextRead "NUM" :: Follower Int) "An integer."
-- >>> runParserIO p ["-n", "42"]
-- 42
--
-- >>> runParserIO p ["-n", "42.0"]
-- <interactive>: command line error at "42.0": Prelude.read: no parse
nextRead :: Read a
         => String
            -- ^ Metavariable for help and error messages.
         -> Follower a
nextRead = addNextHelp R.nextRead

-- | Like 'next' but parses the argument down to a 'Char'. Fails if the
-- argument has length other than 1.
--
-- >>> let p = multiParam ["--pair"] ((,) <$> nextChar "CHAR" <*> nextChar "CHAR") "Two characters."
-- >>> runParserIO p ["--pair", "a", "b"]
-- ('a','b')
--
-- >>> runParserIO p ["--pair", "ab"]
-- <interactive>: command line error at "ab": expected one character, got 2
nextChar :: String
            -- ^ Metavariable for help and error messages.
         -> Follower Char
nextChar = addNextHelp R.nextChar


-- ** Utilities

-- | Adds a @--version@ flag to an existing parser. If @--version@ is on the
-- command line, and is not consumed by the existing parser, the returned
-- wrapper parser will consume the flag and return a @Left@ with the given
-- version information.
--
-- >>> let p = withVersion "Baz v0.1" $ param ["--foo"] "FOO" "Some parameter."
-- >>> runParserIO p ["--foo=bar"]
-- Right "bar"
--
-- >>> runParserIO p ["--version"]
-- Left "Baz v0.1"
withVersion :: String
               -- ^ Version info to be shown to the user.
            -> Parser a
               -- ^ An existing 'Parser'.
            -> Parser (Either String a)
               -- ^ A wrapper 'Parser' that returns either @a@ or the given
               -- version string.
withVersion s pa =
  eject pa $ flag ["--version"] "Show version information and exit." $> s

-- | Like 'withVersion' but doesn't generate help about the @--version@ flag.
withVersion' :: String
                -- ^ Version info to be shown to the user.
             -> Parser a
                -- ^ An existing 'Parser'.
             -> Parser (Either String a)
                -- ^ A wrapper 'Parser' that returns either @a@ or the given
                -- version string.
withVersion' = lift1 . R.withVersion'


-- | Makes an existing 'Parser' stop at @--@. If there is a @--@ on the command
-- line and the existing parser doesn't consume it, the wrapper parser will
-- consume the @--@ and stop.
--
-- You can use this to treat options like @--foo@ as positional arguments. Just
-- wrap all your option parsers in one single 'beforeDashes' and parse the rest
-- with e.g. 'anyArg'.
--
--
-- ==== __Example (arbitrary arguments on both sides of @--@):__
--
-- > -- echo.hs
-- >
-- > import Control.Applicative hiding (many)
-- > import Options.OptStream
-- > ...
-- >
-- > transformP :: Parser (Char -> Char)
-- > transformP
-- >   =   flag' ["-u", "--uppercase"] $> toUpper
-- >   <|> flag' ["-l", "--lowercase"] $> toLower
-- >   <|> orElse id
-- >
-- > main :: IO ()
-- > main = do
-- >   (transform, args) <- parseArgs $ (,)
-- >     <$> beforeDashes transformP
-- >     <#> many (anyArg' "WORD")
-- >
-- >   putStrLn . map transform . concat . intersperse " " $ args
--
-- This @echo@ tool will copy all of its arguments verbatim to stdout, with two
-- exceptions: the first occurrence of flags @-u@, @-uppercase@, @-l@, and
-- @-lowercase@ will make it convert the output to uppercase/lowercase.
--
-- If you want to echo @"--uppercase"@ verbatim, you can use @--@ for that.
-- Note that in this example we use '<#>' to combine the 'beforeDashes' wrapper
-- with 'many' arbitrary arguments, which makes it possible to pass arbitrary
-- arguments on both sides of @--@. Whatever arguments are skipped by
-- @beforeDashes transformP@ will be consumed by @many (anyArg' \"WORD\")@.
--
-- >>> ./echo Hello, world!
-- Hello, world!
--
-- >>> ./echo --uppercase Hello, world!
-- HELLO, WORLD!
--
-- >>> ./echo -- --uppercase Hello, world!
-- --uppercase Hello, world!
--
-- >>> ./echo foo -- bar
-- foo bar
--
-- >>> ./echo foo -- bar -- baz
-- foo bar -- baz
--
-- >>> ./echo --fake-option --
-- --fake-option
--
-- >>> ./echo -- --fake-option
-- --fake-option
--
--
-- ==== __Example (arbitrary arguments to the right of @--@):__
--
-- Now we consider a different example: say we want to have strict syntax to
-- the left of @--@, and arbitrary arguments to the right of @--@. For example,
-- we are writing an interpreter for a scripting language. To the left of @--@
-- we want to pass a number of parameters, as well as positional arguments
-- pointing to the source files of the script. To the right of @--@ we want to
-- pass arbitrary arguments to the script that we are interpreting. We can
-- achieve this by using 'beforeDashes' with sequential application '<*>'.
--
-- > -- dashes.hs
-- >
-- > import Control.Applicative hiding (many)
-- > import Options.OptStream
-- > ...
-- >
-- > -- Options that can show up to the left of '--'.
-- > data Options = Options
-- >   { bool     :: Bool
-- >   , int      :: Int
-- >   , freeArgs :: [String]
-- >   }
-- >
-- > optionsP :: Parser Options
-- > optionsP = Options
-- >   <$> (flag ["-b", "--bool"] "Boolean flag." $> True <|> orElse False)
-- >   <#> (paramRead ["-i", "--int"] "INT" "Integer parameter." <|> orElse 0)
-- >   <#> many (freeArg "LEFT" "Free arguments to the left of --.")
-- >
-- > run :: Options -> [String] -> IO ()
-- > run opts args = do
-- >   putStrLn $ "bool       : " ++ show (bool opts)
-- >   putStrLn $ "int        : " ++ show (int opts)
-- >   putStrLn $ "left of -- : " ++ show (freeArgs opts)
-- >   putStrLn $ "right of --: " ++ show args
-- >
-- > main = join . parseArgsWithHelp
-- >   $ header "Usage: dashes [options] LEFT... [-- RIGHT...]"
-- >   $ sortTable
-- >   $ run
-- >   <$> beforeDashes optionsP
-- >   <*> many (anyArg "RIGHT" "Arguments to the right of --.")
--
-- >>> ./dashes foo -b bar -i 42 baz -- qux
-- bool       : True
-- int        : 42
-- left of -- : ["foo","bar","baz"]
-- right of --: ["qux"]
--
-- >>> ./dashes -- foo -b bar -i 42 baz qux
-- bool       : False
-- int        : 0
-- left of -- : []
-- right of --: ["foo","-b","bar","-i","42","baz","qux"]
--
-- Note that we used the standard applicative '<*>' to combine 'beforeDashes'
-- with 'many'. This way 'many' only starts getting input when 'beforeDashes'
-- is done, i.e. after @--@. The command line is cleanly separated into two
-- parts.  To the left of @--@ we have 'freeArg' that will consume /free/
-- arguments, but will not accept arguments that start with @-@.  To the right
-- of @--@ we have 'anyArg' that will accept anything.
--
-- >>> ./dashes --fake-option
-- dashes: unexpected command line argument "--fake-option"
-- Try "dashes --help" for more information.
--
-- >>> ./dashes -- --fake-option
-- bool       : False
-- int        : 0
-- left of -- : []
-- right of --: ["--fake-option"]
--
-- >>> ./dashes --help
-- Usage: dashes [options] LEFT... [-- RIGHT...]
-- <BLANKLINE>
--   LEFT           Free arguments to the left of --.
--   RIGHT          Arguments to the right of --.
--   -b, --bool     Boolean flag.
--   -i, --int=INT  Integer parameter.
--       --help     Show this help message and exit.
--
-- >>> ./dashes -- --help
-- bool       : False
-- int        : 0
-- left of -- : []
-- right of --: ["--help"]
beforeDashes :: Parser a
                -- ^ An existing 'Parser'.
             -> Parser a
                -- ^ A wrapper that handles @--@.
beforeDashes = lift1 R.beforeDashes


-- * Help

-- | Modifies the 'Help' object stored in a 'Parser' using a given function.
modifyHelp :: (Help -> Help) -> Parser a -> Parser a
modifyHelp mod pa = pa { getHelp = mod $ getHelp pa }

-- | Replaces the 'Help' object stored in a 'Parser' with another one.
setHelp :: Help -> Parser a -> Parser a
setHelp h = modifyHelp $ const h

-- | Convenience helper. Adds a paragraph to the help header. The paragraph is
-- added to the beginning of the existing header, if any.
header :: String -> Parser a -> Parser a
header s = modifyHelp (makeHeader s <>)

-- | Convenience helper. Adds a paragraph to the help footer. The paragraph is
-- added to the beginning of the existing footer, if any.
footer :: String -> Parser a -> Parser a
footer s = modifyHelp (makeFooter s <>)

-- | Convenience helper. Adds a row to the help table describing one flag in
-- the same way as 'flag' does. The row is added to the beginning of the
-- existing table, if any.
--
-- You may pass any number of flag forms (except zero). However, only the first
-- form of each kind (short and long) will appear in the help table.
flagHelp :: [OptionForm]
            -- ^ All flag forms, e.g. @["-f", "--foo"]@.
         -> String
            -- ^ Description (arbitrary string).
         -> Parser a
            -- ^ An existing 'Parser'.
         -> Parser a
            -- ^ The same 'Parser' but with modified help.
flagHelp opts desc = modifyHelp (makeFlagHelp opts desc <>)

-- | Convenience helper. Adds a row to the help table describing one parameter
-- in the same way as 'param' does. The row is added to the beginning of the
-- existing table, if any.
--
-- You may pass any number of parameter forms (except zero). However, only the
-- first form of each kind (short and long) will appear in the help table.
paramHelp :: [OptionForm]
             -- ^ All parameter forms, e.g. @["-f", "--filename"]@.
          -> String
             -- ^ Metavariable, e.g. @\"FILENAME\"@. Can be an arbitrary
             -- string.
          -> String
             -- ^ Description (arbitrary string).
          -> Parser a
             -- ^ An existing 'Parser'.
          -> Parser a
             -- ^ The same 'Parser' but with modified help.
paramHelp opts metavar desc = modifyHelp (makeParamHelp opts metavar desc <>)

-- | Convenience helper. Adds a row to the help table describing one
-- multi-parameter in the same way as 'multiParam' does. The row is added to
-- the beginning of the existing table, if any.
--
-- You may pass any number of parameter forms (except zero). However, only the
-- first form of each kind (short and long) will appear in the help table.
multiParamHelp :: [OptionForm]
               -> String
                  -- ^ All multiparameter forms, e.g. @["-p", "--person"]@.
               -> String
                  -- ^ Follower help string, e.g. @"NAME AGE"@. Can be an
                  -- arbitrary string.
               -> Parser a
                  -- ^ An existing 'Parser'.
               -> Parser a
                  -- ^ The same 'Parser' but with modified help.
multiParamHelp opts fh desc = modifyHelp (makeMultiParamHelp opts fh desc <>)

-- | Convenience helper. Adds a row to the help table describing one free
-- argument in the same way as 'freeArg' does. The row is added to the
-- beginning of the existing table, if any.
freeArgHelp :: String
               -- ^ Metavariable, e.g. @\"FILENAME\"@. Can be an arbitrary
               -- string.
            -> String
               -- ^ Description (arbitrary string).
            -> Parser a
               -- ^ An existing 'Parser'.
            -> Parser a
               -- ^ The same 'Parser' but with modified help.
freeArgHelp metavar desc = modifyHelp (makeFreeArgHelp metavar desc <>)

-- TODO: add customHelp to add a custom row to the table that doesn't fit into
-- the four kinds above.

-- | Empties the 'Help' stored in a given 'Parser'. Shorthand for:
--
-- > clearHelp = setHelp mempty
clearHelp :: Parser a -> Parser a
clearHelp = setHelp mempty

-- | Empties the header portion of the 'Help' object stored in a given
-- 'Parser'.
clearHeader :: Parser a -> Parser a
clearHeader = modifyHelp clearHelpHeader

-- | Empties the footer portion of the 'Help' object stored in a given
-- 'Parser'.
clearFooter :: Parser a -> Parser a
clearFooter = modifyHelp clearHelpFooter

-- | Empties the options table in the 'Help' object stored in a given 'Parser'.
clearTable :: Parser a -> Parser a
clearTable = modifyHelp clearHelpTable

-- | Sorts the options table in the 'Help' object stored in a given 'Parser'.
-- The table is sorted so that free arguments go first and options follow after
-- them.
sortTable :: Parser a -> Parser a
sortTable = modifyHelp sortHelpTable

-- | Adds a @--help@ flag to an existing parser. If the user passes @--help@,
-- and the existing parser doesn't consume it, the returned wrapper parser will
-- return a @Left@ containing a 'Help' object that can be formatted and shown
-- to the user.
--
-- >>> let p = withHelp $ param ["--foo"] "FOO" "Some parameter."
-- >>> runParserIO p ["--foo=bar"]
-- Right "bar"
--
-- >>> runParserIO p ["--help"]
-- Left (Help ...)
--
-- >>> Left help <- runParserIO p ["--help"]
-- >>> putStrLn $ formatHelp help
--   --foo=FOO  Some parameter.
--   --help     Show this help message and exit.
withHelp :: Parser a -> Parser (Either Help a)
withHelp pa = pa' where
  pa' = eject pa $ f $> getHelp pa'
  f = flag ["--help"] "Show this help message and exit."

-- | Like 'withHelp' but doesn't generate help about the @--help@ flag itself.
-- You can use this to replace the built-in "Show this help message and exit"
-- with your own.
--
-- >>> let p = param ["--foo"] "FOO" "Some parameter."
-- >>> let p' = withHelp' . flagHelp ["--help"] "Foo bar baz." $ p
--
-- >>> Left help <- runParserIO p' ["--help"]
-- >>> putStrLn $ formatHelp help
--   --foo=FOO  Some parameter.
--   --help     Foo bar baz.
withHelp' :: Parser a -> Parser (Either Help a)
withHelp' pa = eject pa $ flag' ["--help"] $> getHelp pa

-- | Like 'withHelp' but empties the help of the resulting 'Parser'. Shorthand
-- for:
--
-- > withSubHelp = clearHelp . withHelp
--
-- This can be useful if you want to generate help for subcommands and don't
-- want subcommand options to show up in the main help.
--
-- ==== __Example (subcommands):__
--
-- > import Control.Applicative hiding (optional)
-- > import Options.OptStream
-- >
-- > data Command
-- >   = Send String String
-- >     -- ^ Send email to given recipient with given content.
-- >   | Fetch (Maybe Int)
-- >     -- ^ Fetch emails, with optional count limit.
-- >   deriving Show
-- >
-- > commandP :: Parser (Either Help Command)
-- > commandP = join <$> ( withHelp
-- >   $   header "Usage: email (send | fetch) [options]"
-- >
-- >   $   match "send" *> ( withSubHelp
-- >         $ header "Usage: email send --to=EMAIL BODY"
-- >         $ footer "Example: email send --to=foo@bar.com \'Hello, world!\'"
-- >         $ Send
-- >         <$> param ["--to"] "EMAIL" "Recipient."
-- >         <#> freeArg "BODY" "Email body."
-- >       )
-- >
-- >   <|> match "fetch" *> ( withSubHelp
-- >         $ header "Usage: email fetch [--limit=N]"
-- >         $ footer "Example: email fetch --limit=10"
-- >         $ Fetch
-- >         <$> optional (paramRead ["--limit"] "N" "Limit email count.")
-- >       )
-- >   )
--
-- >>> runParserIO commandP ["send", "--to=foo@bar.com", "Hello, world!"]
-- Right (Send "foo@bar.com" "Hello, world!")
--
-- >>> runParserIO commandP ["fetch", "--limit=42"]
-- Right (Fetch (Just 42))
--
-- >>> Left help <- runParserIO commandP ["--help"]
-- >>> putStrLn . formatHelp $ help
-- Usage: email (send | fetch) [options]
-- <BLANKLINE>
--   --help  Show this help message and exit.
--
-- >>> Left help <- runParserIO commandP ["send", "--help"]
-- >>> putStrLn . formatHelp $ help
-- Usage: email send --to=EMAIL BODY
-- <BLANKLINE>
--   --to=EMAIL  Recipient.
--   BODY        Email body.
--   --help      Show this help message and exit.
-- <BLANKLINE>
-- Example: email send --to=foo@bar.com 'Hello, world!'
--
-- >>> Left help <- runParserIO commandP ["fetch", "--help"]
-- >>> putStrLn . formatHelp $ help
-- Usage: email fetch [--limit=N]
-- <BLANKLINE>
--   --limit=N  Limit email count.
--   --help     Show this help message and exit.
-- <BLANKLINE>
-- Example: email fetch --limit=10
withSubHelp :: Parser a -> Parser (Either Help a)
withSubHelp = clearHelp . withHelp

-- | Like 'withSubHelp' but doesn't generate help about the @--help@ flag itself.
withSubHelp' :: Parser a -> Parser (Either Help a)
withSubHelp' = clearHelp . withHelp

-- ** IO helpers

-- | 'runParserIO' is like 'runParser', except that it terminates the program
-- with 'die' in case of failure. In case of success it returns a pure 'IO'
-- value.
--
-- This is convenient for testing parsers in a REPL:
--
-- >>> runParserIO (param' ["--foo"] "FOO") ["--foo=bar"]
-- "bar"
--
-- >>> runParserIO (param' ["--foo"] "FOO") []
-- <interactive>: missing command line argument: --foo
runParserIO :: Parser a -> [String] -> IO a
runParserIO = R.runParserIO . toRaw

-- | 'parseArgs' is like 'runParserIO', except that it gets the arguments from
-- the environment. You can think of it as a more structured replacement for
-- 'System.Environment.getArgs'.
--
-- > main :: IO ()
-- > main = do
-- >   (src, dst) <- parseArgs $ (,)
-- >     <$> param' ["-i", "--input"] "FILE"
-- >     <#> param' ["-o", "--output"] "FILE"
-- >
-- >   contents <- readFile src
-- >   writeFile dst contents
parseArgs :: Parser a -> IO a
parseArgs = R.parseArgs . toRaw

-- | 'parseArgsWithHelp' is like 'parseArgs', but it also adds a @--help@
-- option to the parser. If the user passes @--help@, 'parseArgsWithHelp' will
-- print the help and exit the program. If there is a parse error, it will
-- print an error message suggesting to use @--help@.
--
-- > main :: IO ()
-- > main = do
-- >   (src, dst) <- parseArgsWithHelp
-- >     $ header "Usage: copy [options]"
-- >     $ footer "Example: copy -i input.txt -o output.txt"
-- >     $ (,)
-- >     <$> param ["-i", "--input"] "FILE" "Input file."
-- >     <#> param ["-o", "--output"] "FILE" "Output file."
-- >
-- >   contents <- readFile src
-- >   writeFile dst contents
--
-- >>> ./copy --help
-- Usage: copy [options]
-- <BLANKLINE>
--   -i, --input=FILE   Input file.
--   -o, --output=FILE  Output file.
--       --help         Show this help message and exit.
-- <BLANKLINE>
-- Example: copy -i input.txt -o output.txt
parseArgsWithHelp :: Parser a -> IO a
parseArgsWithHelp pa = do
  args <- getArgs
  case runParser (withHelp pa) args of
    Right x -> helpToIO x
    Left e -> do
      name <- getProgName
      die $ name ++ ": " ++ R.formatParserError e
            ++ "\nTry \"" ++ name ++ " --help\" for more information."


-- $io-style-parsers
-- Throughout this documentation we call objects of the type @'Parser' ('IO'
-- a)@ /IO-style parsers/. The idea is that instead of parsing command line
-- options into some kind of "options" data structure, and then using that
-- structure to define the behavior of our program, we can parse the command
-- line directly into the IO action that defines the behavior. Consider this
-- (somewhat artificial) example:
--
-- @
-- module Main where
-- 
-- import Control.Applicative
-- import Control.Monad
-- import "Options.OptStream"
-- 
-- copy :: String -> String -> IO ()
-- copy src dst = do
--   contents <- readFile src
--   writeFile dst contents
-- 
-- main :: IO ()
-- main = 'join' . 'parseArgsWithHelp'
--   $ 'header' "Usage: copy -i FILE -o FILE"
--   $ copy
--   '<$>' 'param' ["-i", "--input"] "FILE" "Input file."
--   '<#>' 'param' ["-o", "--output"] "FILE" "Output file."
-- @
--
-- The program has two command line options: an input and an output file. It
-- never stores them in any data structurre: rather, they are passed directly
-- to the function @copy@ using '<$>', resulting in an IO-style parser:
--
-- > copy <$> param ... <#> param ... :: Parser (IO ())
--
-- Note how this parser is then executed:
--
-- > join . parseArgsWithHelp :: Parser (IO a) -> IO a
--
-- This composition @(join . parseArgsWithHelp)@ returns an IO action that does
-- all of the following:
--
--   * Extracts command line arguments from the environment.
--   * Parses them, handling errors and @--help@.
--   * Executes the @IO a@ action that resulted from the parse (this part is
--   accomplished by 'join').
--
-- You don't have to use IO-style parsers if you don't want to, but if you do
-- then you may find the helper functions below useful.
--
-- ==== __Demo outputs:__
--
-- >>> echo baz > foo.txt
-- >>> ./copy -i foo.txt -o bar.txt
-- >>> cat bar.txt
-- baz
--
-- >>> ./copy --help
-- Usage: copy -i FILE -o FILE
-- <BLANKLINE>
--   -i, --input=FILE   Input file.
--   -o, --output=FILE  Output file.
--       --help         Show this help message and exit.


helpToIO :: Either Help a -> IO a
helpToIO (Right a) = return a
helpToIO (Left h) = do
  putStrLn $ formatHelp h
  exitSuccess

-- | Adds help to an IO-style 'Parser'. It theere is @--help@ on the command
-- line and the existing 'Parser' doesn't consume it, then the created wrapper
-- will return an 'IO' action that prints the help and exits the program.
-- Otherwise the existing parser will produce an 'IO' action to run the program
-- as usual.
--
-- If you are using 'parseArgsWithHelp', that will already take care of all the
-- above. However, sometimes you may still want to use 'withHelpIO' or
-- 'withSubHelpIO' to deal with subcommands, or in other special cases.
withHelpIO :: Parser (IO a)
              -- ^ An existing IO-style 'Parser'.
           -> Parser (IO a)
              -- ^ A wrapper that handles @--help@.
withHelpIO = fmap (join . helpToIO) . withHelp

-- | Like 'withHelpIO' but doesn't generate help about the added @--help@ flag
-- itself. You can use this e.g. if you don't like the standard "Show this help
-- message and exit" text.
--
-- ==== __Example (custom help):__
--
-- > hello :: String -> IO ()
-- > hello name = putStrLn $ "Hello, " ++ name ++ "!"
-- >
-- > main :: IO ()
-- > main = join . parseArgs
-- >   $ withHelpIO'
-- >   $ flagHelp ["--help"] "Print this special help message!"
-- >   $ header "Usage: hello [NAME]"
-- >   $ hello <$> (freeArg' "NAME" <|> orElse "James Bond")
--
-- >>> ./hello
-- Hello, James Bond!
--
-- >>> ./hello --help
-- Usage: hello [NAME]
-- <BLANKLINE>
--   --help  Print this special help message!
withHelpIO' :: Parser (IO a)
               -- ^ An existing IO-style 'Parser'.
            -> Parser (IO a)
               -- ^ A wrapper that handles @--help@.
withHelpIO' = fmap (join . helpToIO) . withHelp'

-- | Like 'withHelpIO' but empties the help of the returned wrapper 'Parser'.
-- Equivalent to
--
-- > clearHelp . withHelpIO
--
-- This can be useful if you want to generate help for subcommands and don't
-- want subcommand options to show up in the main help.
--
-- ==== __Example (subcommands):__
--
-- > import Control.Applicative hiding (optional)
-- > import Options.OptStream
-- >
-- > send :: String -> String -> IO ()
-- > send src dst = putStrLn $ "Would send " ++ show dst ++ " to " ++ src ++ "."
-- >
-- > fetch :: Maybe Int -> IO ()
-- > fetch Nothing = putStrLn $ "Would fetch all emails."
-- > fetch (Just n) = putStrLn $ "Would fetch at most " ++ show n ++ " emails."
-- >
-- > main :: IO ()
-- > main = join . parseArgsWithHelp
-- >   $   header "Usage: email (send | fetch) [options]"
-- >
-- >   $   match "send" *> ( withSubHelpIO
-- >         $ header "Usage: email send --to=EMAIL BODY"
-- >         $ footer "Example: email send --to=foo@bar.com \'Hello, world!\'"
-- >         $ send
-- >         <$> param ["--to"] "EMAIL" "Recipient."
-- >         <#> freeArg "BODY" "Email body."
-- >       )
-- >
-- >   <|> match "fetch" *> ( withSubHelpIO
-- >         $ header "Usage: email fetch [--limit=N]"
-- >         $ footer "Example: email fetch --limit=10"
-- >         $ fetch
-- >         <$> optional (paramRead ["--limit"] "N" "Limit email count.")
-- >       )
--
-- >>> ./email send --to=foo@bar.com 'Hello, world!'
-- Would send "Hello, world!" to foo@bar.com.
--
-- >>> ./email fetch
-- Would fetch all emails.
--
-- >>> ./email --help
-- Usage: email (send | fetch) [options]
-- <BLANKLINE>
--   --help  Show this help message and exit.
--
-- >>> ./email send --help
-- Usage: email send --to=EMAIL BODY
-- <BLANKLINE>
--   --to=EMAIL  Recipient.
--   BODY        Email body.
--   --help      Show this help message and exit.
-- <BLANKLINE>
-- Example: email send --to=foo@bar.com 'Hello, world!'
--
-- >>> ./email fetch --help
-- Usage: email fetch [--limit=N]
-- <BLANKLINE>
--   --limit=N  Limit email count.
--   --help     Show this help message and exit.
-- <BLANKLINE>
-- Example: email fetch --limit=10
withSubHelpIO :: Parser (IO a)
                 -- ^ An existing IO-style 'Parser'.
              -> Parser (IO a)
                 -- ^ A wrapper that handles @--help@.
withSubHelpIO = fmap (join . helpToIO) . withSubHelp

-- | Like 'withSubHelpIO' but doesn't generate help about the added @--help@
-- flag itself.
withSubHelpIO' :: Parser (IO a)
                  -- ^ An existing IO-style 'Parser'.
               -> Parser (IO a)
                  -- ^ A wrapper that handles @--help@.
withSubHelpIO' = fmap (join . helpToIO) . withSubHelp'

-- | Adds a @--version@ flag to an existing IO-style 'Parser'. If the user
-- passes @--version@ on the command line and the existing parser doesn't
-- consume this flag, the wrapper will consume it and return an 'IO' action
-- that prints version information and exits. Otherwise the wrapper will let
-- the existing parser finish the parse normally.
--
--
-- ==== __Example:__
--
-- > hello :: String -> IO ()
-- > hello name = putStrLn $ "Hello, " ++ name ++ "!"
-- >
-- > main :: IO ()
-- > main = join . parseArgsWithHelp
-- >   $ withVersionIO "Hello, version 1.0"
-- >   $ header "Usage: hello [NAME]"
-- >   $ footer "Example: hello \'Sherlock Holmes\'"
-- >   $ hello
-- >   <$> (freeArg "NAME" "Your name (optional)." <|> orElse "James Bond")
--
-- >>> ./hello
-- Hello, James Bond!
--
-- >>> ./hello --version
-- Hello, version 1.0
--
-- >>> ./hello --help
-- Usage: hello [NAME]
-- <BLANKLINE>
--   NAME       Your name (optional).
--   --version  Show version information and exit.
--   --help     Show this help message and exit.
-- <BLANKLINE>
-- Example: hello 'Sherlock Holmes'
withVersionIO :: String
                 -- ^ Version information to show to the user.
              -> Parser (IO a)
                 -- ^ An existing 'Parser'.
              -> Parser (IO a)
                 -- ^ A wrapper that handles @--version@.
withVersionIO s = fmap (join . versionToIO) . withVersion s

-- | Like 'withVersionIO' but doesn't generate help about the @--version@ flag.
withVersionIO' :: String
                  -- ^ Version information to show to the user.
               -> Parser (IO a)
                  -- ^ An existing 'Parser'.
               -> Parser (IO a)
                  -- ^ A wrapper that handles @--version@.
withVersionIO' = lift1 . R.withVersionIO'


-- TODO: Check laws for all instances in this file.
-- TODO: Figure out which laws there are in SubstreamParser.
-- TODO: (?) Identify what type of grammars we are parsing here, exactly. Does
--       it fit the established classification?

-- TODO: check that REPL and code examples in Haddock run as advertised.
-- TODO: (eventually) Figure out what other compilers we can run on, add
--       "Portability" field to module descriptions.
-- TODO: (?) find out how to integrate with bash auto-complete.

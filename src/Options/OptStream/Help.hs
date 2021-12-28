{-|
Module      : Options.OptStream.Help
Copyright   : (c) Dan Shved, 2021
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

This module contains lower-level functions for working with 'Help' objects. You
may need to import this if you work with 'Help' objects directly, as opposed to
relying on them being handled automatically by 'Options.OptStream.Parser'.
-}
module Options.OptStream.Help
  ( -- * Help objects
    Help
  , formatHelp
  , makeHeader
  , makeFooter
  , makeFlagHelp
  , makeParamHelp
  , makeMultiParamHelp
  , makeFreeArgHelp

    -- * Modifiers
  , clearHelpHeader
  , clearHelpFooter
  , clearHelpTable
  , sortHelpTable
  )
where

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord

import Options.OptStream.Internal hiding (many)
import qualified Options.OptStream.Internal as I

-- One row in the --help table.
data OptionHelp
  = FlagHelp
    { ohShort        :: Maybe Char
    , ohLong         :: Maybe String
    , ohDescription  :: String
    }
  | ParamHelp
    { ohShort        :: Maybe Char
    , ohLong         :: Maybe String
    , ohMetavar      :: String
    , ohDescription  :: String
    }
  | MultiParamHelp
    { ohShort        :: Maybe Char
    , ohLong         :: Maybe String
    , ohFollowerHelp :: String
    , ohDescription  :: String
    }
  | FreeArgHelp
    { ohMetavar      :: String
    , ohDescription  :: String
    }
  deriving (Eq, Show)


-- | Represents help information that could be printed when the user passes
-- @--help@ on the command line.
--
-- A 'Help' object contains three parts, each of which could be empty: a
-- header, an options table, and a footer. 'Help' objects can be composed
-- together using '<>'. That will separately concatenate headers, option
-- tables, and footers.
data Help = Help
  { helpTable :: List OptionHelp
  , helpHeader  :: List String
  , helpFooter  :: List String
  }
  deriving (Eq, Show)

instance Semigroup Help where
  Help x y z <> Help x' y' z' = Help (x <> x') (y <> y') (z <> z')

instance Monoid Help where
  mempty = Help mempty mempty mempty

-- | Makes a 'Help' object that contains one paragraph in the header.
makeHeader :: String -> Help
makeHeader s = Help mempty (single s) mempty

-- | Makes a 'Help' object that contains one paragraph in the footer.
makeFooter :: String -> Help
makeFooter s = Help mempty mempty (single s)

getShortAndLong :: [OptionForm] -> (Maybe Char, Maybe String)
getShortAndLong opts = (listToMaybe shorts, listToMaybe longs) where
  (shorts, longs) = foldr update ([], []) $ map parseOptionForm opts
  update (Short c) (cs, ss) = (c:cs, ss)
  update (Long s) (cs, ss) = (cs, s:ss)

-- | Makes a 'Help' object that contains one row in the options table. This
-- function is suitable to add 'Help' to a flag, i.e. an option that doesn't
-- take any additional arguments.
--
-- You may pass any number of option forms. However, only the first one of each
-- kind (short and long) will be used.
--
-- >>> formatHelp $ makeFlagHelp ["-f", "--foo"] "Description."
-- "  -f, --foo  Description."
makeFlagHelp :: [OptionForm]
                -- ^ All the flag forms, e.g. @["-f", "--foo"]@.
             -> String
                -- ^ Description.
             -> Help
makeFlagHelp opts desc = Help (single oh) mempty mempty where
  oh = uncurry FlagHelp (getShortAndLong opts) desc

-- | Makes a 'Help' object that contains one row in the options table. This
-- function is suitable to add 'Help' to a parameter, i.e. an option that takes
-- one additional argument.
--
-- You may pass any number of option forms. However, only the first one of each
-- kind (short and long) will be used.
--
-- >>> formatHelp $ makeParamHelp ["-i", "--input"] "FILE" "Input file."
-- "  -i, --input=FILE  Input file."
makeParamHelp :: [OptionForm]
                 -- ^ All parameter forms, e.g. @["-i", "--input"]@.
              -> String
                 -- ^ Metavariable describing the additional argument, e.g.
                 -- @\"FILE\"@.
              -> String
                 -- ^ Description.
              -> Help
makeParamHelp opts metavar desc = Help (single oh) mempty mempty where
  oh = uncurry ParamHelp (getShortAndLong opts) metavar desc

-- | Makes a 'Help' object that contains one row in the options table. This
-- function is suitable to add 'Help' to a multi-parameter, i.e. an option that
-- takes an arbitrary number of additional arguments.
--
-- In practice this behaves almost the same as 'makeParamHelp', except it
-- advertises a slightly different syntax for passing additional arguments: as
-- proper additional arguments, without @'='@.
--
-- You may pass any number of option forms. However, only the first one of each
-- kind (short and long) will be used.
--
-- >>> formatHelp $ makeMultiParamHelp ["-n", "--full-name"] "FIRST LAST" "First and last name."
-- "  -n, --full-name FIRST LAST  First and last name."
makeMultiParamHelp :: [OptionForm]
                      -- ^ All multiparameter forms, e.g. @["-n",
                      -- "--full-name"]@.
                   -> String
                      -- ^ Free-form description for the additional arguments,
                      -- e.g. @"FIRST LAST"@.
                   -> String
                      -- ^ Description.
                   -> Help
makeMultiParamHelp opts fh desc = Help (single oh) mempty mempty where
  oh = uncurry MultiParamHelp (getShortAndLong opts) fh desc

-- | Makes a 'Help' object that contains one row in the options table. This
-- function is suitable to add 'Help' to a free argument.
--
-- >>> formatHelp $ makeFreeArgHelp "FILE" "Input file."
-- "  FILE  Input file."
makeFreeArgHelp :: String
                   -- ^ Metavariable, e.g. @\"FILE\"@.
                -> String
                   -- ^ Description.
                -> Help
makeFreeArgHelp metavar desc = Help (single oh) mempty mempty where
  oh = FreeArgHelp metavar desc

-- | Clears the header of a 'Help' object. Doesn't affect the options table and
-- the footer.
clearHelpHeader :: Help -> Help
clearHelpHeader h = h { helpHeader = mempty }

-- | Clears the footer of a 'Help' object. Doesn't affect the header and the
-- options table.
clearHelpFooter :: Help -> Help
clearHelpFooter h = h { helpFooter = mempty }

-- | Clears the options table of a 'Help' object. Doesn't affect the header and
-- the footer.
clearHelpTable :: Help -> Help
clearHelpTable h = h { helpTable = mempty }


-- * Sorting

-- Favor rows with data when sorting.
compareStr :: String -> String -> Ordering
compareStr (_:_) [] = LT
compareStr [] (_:_) = GT
compareStr x y = compare x y

instance Ord Row where
  compare (FreeArgRow _ _) (OptionRow _ _ _) = LT
  compare (OptionRow _ _ _) (FreeArgRow _ _) = GT
  compare (FreeArgRow x y) (FreeArgRow x' y') =
    compareStr x x' <> compareStr y y'
  compare (OptionRow x y z) (OptionRow x' y' z') =
    compareStr x x' <> compareStr y y' <> compareStr z z'

instance Ord OptionHelp where
  compare = compare `on` toRow

-- | Sorts the options table so that:
--
--   * Free argument options go first, proper options go second.
--
--   * Free arguments are sorted lexicographically by metavariable, then by
--   description.
--
--   * Options are sorted lexicographically by short form, then by long form,
--   then by description.
sortHelpTable :: Help -> Help
sortHelpTable h =
  h {helpTable = I.fromList . sortOn toRow . toList $ helpTable h}


-- * Formatting

pad :: Int -> String -> String
pad n s
  | l < n = s ++ replicate (n - l) ' '
  | otherwise = s
  where l = length s

formatShort :: Maybe Char -> String
formatShort (Just c) = ['-', c]
formatShort Nothing = []

formatLong :: Maybe String -> String
formatLong (Just s) = "--" ++ s
formatLong Nothing = ""

formatShortParam :: Char -> String -> String
formatShortParam c v = ['-', c, ' '] ++ v

formatLongParam :: String -> String -> String
formatLongParam s v = "--" ++ s ++ "=" ++ v

formatShortMulti :: Char -> String -> String
formatShortMulti c h = ['-', c, ' '] ++ h

formatLongMulti :: String -> String -> String
formatLongMulti s h = "--" ++ s ++ " " ++ h

-- One row in a --help table.
data Row
  = FreeArgRow String String
  | OptionRow String String String
  deriving Eq

toRow :: OptionHelp -> Row
toRow (FlagHelp mbShort mbLong desc) =
  OptionRow (formatShort mbShort) (formatLong mbLong) desc

toRow (ParamHelp mbShort (Just s) v desc) =
  OptionRow (formatShort mbShort) (formatLongParam s v) desc
toRow (ParamHelp (Just c) Nothing v desc) =
  OptionRow (formatShortParam c v) "" desc
toRow (ParamHelp Nothing Nothing _ desc) =
  OptionRow "" "" desc

toRow (MultiParamHelp mbShort (Just s) h desc) =
  OptionRow (formatShort mbShort) (formatLongMulti s h) desc
toRow (MultiParamHelp (Just c) Nothing h desc) =
  OptionRow (formatShortMulti c h) "" desc
toRow (MultiParamHelp Nothing Nothing _ desc) =
  OptionRow "" "" desc

toRow (FreeArgHelp v desc) =
  FreeArgRow v desc


formatOptionHelp :: [OptionHelp] -> String
formatOptionHelp hs = concat . intersperse "\n" $ outputs where
  rows = map toRow hs
  shortWidth = min 2 $ foldr max 0 [length s | OptionRow s _ _ <- rows]
  hasShort = shortWidth /= 0
  longWidth = foldr max 0 [length l | OptionRow _ l _ <- rows]
  hasLong = longWidth /= 0
  commaWidth = if hasShort && hasLong then 2 else 0

  toPair (FreeArgRow s d) = (s, d)
  toPair (OptionRow s "" d) = (s, d)
  toPair (OptionRow s l d) = (shortStr ++ commaStr ++ l, d) where
    shortStr = pad shortWidth s
    commaStr = pad commaWidth $ if null s then "" else ","

  pairs = map toPair rows
  optWidth = foldr max 0 . filter (<= 30) $ map (length . fst) pairs

  getOutputStrs (opt, desc) =
    ("  " ++ (pad optWidth opt) ++ "  " ++ d)
    :[replicate (optWidth + 4) ' ' ++ d' | d' <- ds]
    where
      (d, ds) = case lines desc of
        [] -> ("", [])
        (d:ds) -> (d, ds)

  outputs = concatMap getOutputStrs pairs

-- | Formats the 'Help' object.
--
-- > h :: Help
-- > h = makeHeader "Usage: program [options] ARG"
-- >  <> makeFreeArgHelp "ARG" "Positional argument."
-- >  <> makeFlagHelp ["-f", "--foo"] "A flag."
-- >  <> makeParamHelp ["-p", "--param"] "VAL" "A parameter."
-- >  <> makeFooter "Example: program --foo bar"
--
-- >>> putStrLn $ formatHelp h
-- Usage: program [options] ARG
-- <BLANKLINE>
--   ARG              Positional argument.
--   -f, --foo        A flag.
--   -p, --param=VAL  A parameter.
-- <BLANKLINE>
-- Example: program --foo bar
formatHelp :: Help -> String
formatHelp h = concat . intersperse "\n\n" $ concat
  [ toList $ helpHeader h
  , case toList $ helpTable h of
      [] -> []
      hs -> [formatOptionHelp . toList $ helpTable h]
  , toList $ helpFooter h
  ]


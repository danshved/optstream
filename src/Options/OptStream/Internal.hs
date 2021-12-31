{-|
Module      : Options.OptStream.Internal
Copyright   : (c) Dan Shved, 2021
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

Internal helpers for the optstream library. Should not be imported directly
from the outside.
-}
module Options.OptStream.Internal where

import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Prelude hiding (putStrLn)

import Options.OptStream.IOOps


-- * Quickly composable lists

newtype List a = List ([a] -> [a])

single :: a -> List a
single a = List (a:)

fromList :: [a] -> List a
fromList as = List (as++)

instance Semigroup (List a) where
  List f <> List g = List $ f . g

instance Monoid (List a) where
  mempty = List id

instance Foldable List where
  foldr comb acc (List f) = foldr comb acc $ f []
  toList (List f) = f []

instance Show a => Show (List a) where
  showsPrec d l = showParen (10 < d) $
    showString "fromList " . showsPrec 11 (toList l)

instance Eq a => Eq (List a) where
  (==) = (==) `on` toList


-- * Removing duplicates

nubOrd :: Ord a => [a] -> [a]
nubOrd = work . sort where
  work (x:xs@(y:ys))
    | x == y = work $ x:ys
    | otherwise = x:work xs
  work xs = xs


-- * Option forms

-- | High-level option parsers all accept a list of /option forms/. An option
-- form is simply a 'String'.
--
-- There are two kinds of legal option forms: /short forms/, e.g. @"-f"@, and
-- /long forms/, e.g. @"--foo"@. Any function that accepts an 'OptionForm' will
-- fail with an 'error' if the option form is illegal. See 'isLegalOptionForm'.
type OptionForm = String

data Option = Short Char | Long String
  deriving Show

parseOptionForm_ :: OptionForm -> Maybe Option
parseOptionForm_ ('-':c:[]) | c /= '-' = Just $ Short c
parseOptionForm_ ('-':'-':s@(_:_)) = Just $ Long s
parseOptionForm_ s@_ = Nothing

-- | Checks whether the given string is a legal option form. A legal short form
-- is @-C@, where @C@ is any character other than @-@. A legal long form is
-- @--STR@, where @STR@ is any non-empty string.
--
-- This function is here just in case. Normally the programmer will provide
-- option forms as string literals, so they will probably be legal.
--
-- ==== __Example:__
-- >>> isLegalOptionForm "-f"
-- True
-- >>> isLegalOptionForm "--foo"
-- True
-- >>> isLegalOptionForm "bar"
-- False
-- >>> isLegalOptionForm ""
-- False
-- >>> isLegalOptionForm "-"
-- False
-- >>> isLegalOptionForm "--"
-- False
-- >>> isLegalOptionForm "---"
-- True
isLegalOptionForm :: OptionForm -> Bool
isLegalOptionForm = isJust . parseOptionForm_

parseOptionForm :: OptionForm -> Option
parseOptionForm s = case parseOptionForm_ s of
  Just x -> x
  Nothing -> error $ "illegal option form " ++ show s


-- * Miscellaneous

versionToIO :: IOOps m => Either String a -> m a
versionToIO (Right a) = return a
versionToIO (Left s) = do
  putStrLn s
  exitSuccess


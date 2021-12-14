{-|
Module      : Options.OptStream.Classes
Copyright   : (c) Dan Shved, 2021
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

This module contains the 'SubstreamParser' typeclass. Substream parsers can be
composed in many ways, notably using sequential application '<*>' as well as
parallel application '<#>', which makes them /twice applicative/. See the
typeclass documentation for more details.

Normally you shouldn't need to import this module as it is re-exported by
"Options.OptStream".
-}
module Options.OptStream.Classes
  ( -- * Twice applicative parsers
    SubstreamParser(..)
  , orElse
  , applyMany
  , applySome
  , (<#)
  , (#>)
  , (<##>)
  , (<-#)
  , (-#>)
  , (<-##>)
  , (<#-)
  , (#->)
  , (<##->)

    -- * Functors and Applicatives with failure
  , FunctorFail(fmapOrFail)
  , fmapOrFailM
  , (<$?>)
  , (<&?>)
  , ApplicativeFail(failA)
  )
where

import Control.Applicative hiding (some, many)
import Control.Arrow
import Data.Functor


-- * FunctorFail

-- | A functor with failure. Instances are expected to satisfy the laws:
--
-- Coordinated with 'fmap':
--
-- prop> fmapOrFail (Right . f) = fmap f
--
-- Composition:
--
-- prop> fmapOrFail f . fmapOrFail g = fmapOrFail (g >=> f)
--
-- Additionally, if @f@ is a 'Monad', it is expected that failure is the same
-- as the monadic 'fail':
--
-- prop> fmapOrFail Left (return err) = fail err
class Functor f => FunctorFail f where
  -- | Like 'fmap' but with a possibility for failure. This applies a function
  -- of type @(a -> 'Either' 'String' b)@ to a value of type @f a@. If the
  -- function returns a 'Left', this represents failure, with the 'String'
  -- being a message describing the nature of the failure. In this case the
  -- resulting value of type @f b@ should encapsulate this failure.
  --
  -- In command line parsing this is used by functions like
  -- 'Options.OptStream.paramRead' to produce an unrecoverable failure when a
  -- parameter cannot be parsed e.g. as an integer.
  fmapOrFail :: (a -> Either String b) -> f a -> f b

-- | Like 'Control.Monad.liftM' but with a possibility for failure. Suitable as
-- a drop-in implementation of 'fmapOrFail' for monads.
fmapOrFailM :: Monad f => (a -> Either String b) -> f a -> f b
fmapOrFailM f a = either fail return . f =<< a

infixl 4 <$?>
infixl 1 <&?>

-- | Operator form of 'fmapOrFail'. Provided for convenience.
(<$?>) :: FunctorFail f => (a -> Either String b) -> f a -> f b
(<$?>) = fmapOrFail

-- | A version '<$?>' with the arguments flipped. Provided for convenience.
(<&?>) :: FunctorFail f => f a -> (a -> Either String b) -> f b
(<&?>) = flip (<$?>)


-- * ApplicativeFail

-- | An 'Applicative' that is also a 'FunctorFail'. Instances are expected to
-- satisfy the law:
--
-- prop> failA err = fmapOrFail Left (pure err)
class (Applicative f, FunctorFail f) => ApplicativeFail f where
  -- | Unconditional failure.
  failA :: String -> f a
  failA = fmapOrFail Left . pure


-- * SubstreamParser

infixl 4 <#>
infixl 4 <#
infixl 4  #>
infixl 4 <##>

infixl 4 <-#>
infixl 4 <-#
infixl 4  -#>
infixl 4 <-##>

infixl 4 <#->
infixl 4 <#-
infixl 4  #->
infixl 4 <##->

infixl 3 <-|>
infixl 3 <|->

-- | A type @p@ is a 'SubstreamParser' if it offers functions 'eof', '<#>',
-- '<-#>', '<#->', '<-|>', '<|->', standard 'Applicative' functions 'pure' and
-- '<*>', and 'Alternative' functions 'empty' and '<|>'.
--
-- A substream parser handles a stream of /tokens/ of some kind. In case of
-- 'Options.OptStream.Parser' and 'Options.OptStream.RawParser', there are two
-- kinds of tokens: command line arguments and short flags (which are
-- characters inside command line arguments starting with one @-@). Common to
-- all substream parsers is also a special EOF token that is received at the
-- end of the stream.
--
-- A substream parser looks at each token, including EOF, and makes one of
-- these decisions:
--
--  * /Skip/ the token, in which case the token may be consumed by a different
--  parser. Hence the name /substream parser/: a parser only handles part of
--  the stream and skips the rest.
--
--  * /Consume/ the token, in which case there are generally two subchoices:
--
--      - Finish the parse (and return a value of type @a@, or maybe an
--      error).
--
--      - Continue the parse, i.e. keep looking at more tokens.
--
--
-- ==== Applicative
--
-- The meaning of 'Applicative' functions for substream parsers:
--
-- [@'pure' a@]: A parser that finishes immediately before looking at any tokens
-- and produces the value @a@.
--
-- [@f '<*>' x@]: Sequential application. The left hand side parser @f@ runs
-- first until it finishes and produces a value of type @(a -> b)@. Then the
-- right hand side parser @x@ starts running and looking at the remainder of
-- the stream. Once @x@ has finished and produced a value of type @a@, the
-- results of @f@ and @x@ are combined into a final value of type @b@.
--
--
-- ==== Alternative
--
-- [@'empty'@]:  A parser that skips all tokens, including EOF, and never
-- finishes.
--
-- [@x '<|>' y@]:  Alternative. The combined parser looks at each token and
-- determines if either of the subparsers wants to consume it (with @x@ having
-- priority over @y@). When (say) @x@ has consumed the first token, @y@ is
-- terminated, and @x@ gets to look at the rest of the stream. This implies
-- that there is no backtracking. Once either @x@ or @y@ has cosnumed a token,
-- the decision has been made and cannot be reversed.
--
-- [@many@, @some@, @optional@]:  These functions from "Control.Applicative"
-- use 'pure' as a fallback alternative. This doesn't work for substream
-- parsers (see 'eof'), so this module provides its own replacements with the
-- same names: 'Options.OptStream.Classes.many',
-- 'Options.OptStream.Classes.some', 'Options.OptStream.Classes.optional'.
class Alternative p => SubstreamParser p where
  -- | Skips all input tokens except EOF, then produces the given value upon
  -- consuming EOF.
  --
  -- Note that this has the same type signature as 'pure'. Both 'eof' and
  -- 'pure' produce the same parse result. However, 'pure' finishes
  -- immediately, whereas 'eof' waits until the end of the stream.
  --
  -- Most of the time this makes 'eof' the correct choice to use in
  -- 'Alternative' expressions in order to provide default values:
  --
  -- @
  -- p '<|>' 'eof' a
  -- @
  --
  -- This will try running the parser @p@, and if @p@ never consumes any input
  -- at all (including the EOF token) and never finishes, produce @a@ as the
  -- default result.  Because of this pattern 'eof' has a synonym called
  -- 'orElse'.
  --
  -- If we were to use 'pure' instead of 'eof' above, it wouldn't work. The
  -- 'pure' alternative would immediately "win" and kill @p@ (unless @p@ also
  -- finishes immediately before looking at any tokens).
  eof :: a -> p a

  -- | Parallel application. This runs two parsers in parallel. For each input
  -- token, the left hand side (LHS) parser looks at it first. If the LHS
  -- parser skips the token, the right hand side (RHS) parser gets a chance to
  -- look at it too. Once both the LHS and RHS parsers have finished and
  -- produced results of type @(a -> b)@ and @a@ respectively, the compound
  -- parser also finishes and produces a combined result of type @b@.
  (<#>) :: p (a -> b) -> p a -> p b

  -- | Left-interruptive application. This starts out in the same way as '<#>':
  -- each token is presented to the left hand side (LHS) parser first, then to
  -- the right hand side (RHS) parser if the LHS one has skipped it. However,
  -- as soon as the RHS parser has consumed a token, the LHS parser is
  -- terminated.
  --
  -- The LHS parser is terminated gracefully (it looks to the LHS parser as if
  -- it received EOF). The LHS result is stored, and the RHS parser continues
  -- to run alone. When the RHS parser finishes, the LHS and RHS results are
  -- combined.
  --
  -- In command line parsing, this is used by 'Options.OptStream.beforeDashes'.
  -- The LHS in this case is the main options parser for an application, and
  -- the RHS matches @"--"@. If the user passes @"--"@ on the command line, it
  -- looks like EOF to the main options parser.
  (<-#>) :: p (a -> b) -> p a -> p b

  -- | Right-interruptive application. Similar to '<-#>' but with the roles
  -- reversed: once the left hand side (LHS) parser consumes a token, the right
  -- hand side (RHS) is terminated as if by reaching EOF.
  --
  -- Note however that both in '<-#>' and '<#->' the LHS parser looks at each
  -- token first, and the RHS parser gets to look at it only if the LHS one has
  -- skipped it.
  (<#->) :: p (a -> b) -> p a -> p b

  -- | Left-interruptive alternative. This starts off running two parsers in
  -- parallel. For each input token, the left hand side (LHS) parser looks at
  -- it first. If the LHS parser skips a token, the right hand side (RHS)
  -- parser gets a chance to look at it.
  --
  -- If the LHS parser finishes before the RHS one consumes any tokens, the LHS
  -- parser "wins": its result becomes the result of the compound parser.
  --
  -- If, on the other hand, the RHS parser consumes a token before the LHS
  -- parser has finished, the LHS parser is terminated. The state of the LHS
  -- parser is discarded, all the tokens it has consumed are lost. The RHS
  -- parser takes over and gets to finish the parse.
  --
  -- In command line parsing, this is used by 'Options.OptStream.withHelp' to
  -- implement the @--help@ flag. In this case the LHS is the main options
  -- parser for an application, and the RHS matches @"--help"@. If the user
  -- passes @"--help"@ anywhere on the command line, the normal argument
  -- parsing gets interrupted and help information is returned instead.
  (<-|>) :: p a -> p a -> p a

  -- | Right-interruptive alternative. Similar to '<-|>' but with the roles
  -- reversed: if the left hand side (LHS) parser consumes a token before the
  -- right hand side (RHS) one has finished, the RHS parser is terminated.
  --
  -- Note however that both in '<-|>' and '<|->' the LHS parser looks at each
  -- token first, and the RHS parser gets to look at it only if the LHS one has
  -- skipped it.
  (<|->) :: p a -> p a -> p a

  -- | Zero or more. This will run the given parser repeatedly until EOF is
  -- reached and gather results in a list.
  many :: p a -> p [a]
  many pa = pas where
    pas = ((:) <$> pa <*> pas) <|> orElse []

  -- | One or more. This will run the given parser repeatedly until EOF is
  -- reached and gather results in a list. Will refuse to consume EOF unless
  -- at least one item has been parsed.
  some :: p a -> p [a]
  some pa = (:) <$> pa <*> many pa

  -- | Zero or one. This will try running the given parser, but will return
  -- 'Nothing' if the parser never consumes any tokens.
  optional :: p a -> p (Maybe a)
  optional pa = Just <$> pa <|> orElse Nothing

  -- | Bounded number of items. @'between' l u x@ will try running the parser
  -- @x@ at least @l@ times in sequence, and fail if EOF is reached before @l@
  -- items have been parsed.
  --
  -- Will finish the parse when either EOF is reached or @u@ items have been
  -- parsed.
  between :: Int
             -- ^ Lower bound (l).
          -> Int
             -- ^ Upper bound (u).
          -> p a
          -> p [a]
  between min max pa
    | 0 > min = error "between: min is negative"
    | min > max = error "between: min > max"
    | otherwise = build min max where
      build 0 0 = pure []
      build 0 n = (:) <$> pa <*> build 0 (n - 1) <|> orElse []
      build min max = (:) <$> pa <*> build (min - 1) (max - 1)

  -- | Permutation. Will try to parse one item by each of the given parsers, in
  -- any order.
  --
  -- Just like '<|>' this doesn't offer any back-tracking. All the parsers will
  -- get a chance to look at each input token one after another.  When one of
  -- the parsers consumes a token, that parser will run until it finishes the
  -- parse. Then the remaining parsers will continue the process. When all the
  -- parsers have produced a value the compound parser will finish and produce
  -- a list of results. The returned items will be in the same order as they
  -- appear in the stream.
  --
  -- In general, this is less convenient than simply combining all your parsers
  -- with '<#>'. However, you can use 'perm' if you care about the order in
  -- which the items appear in the stream.
  perm :: [p a] -> p [a]
  perm [] = pure []
  perm xs = enumerate [] xs where
    enumerate _ [] = empty
    enumerate ys' (y:ys)
      =   ((:) <$> y <*> perm (reverse ys' ++ ys))
      <|> enumerate (y:ys') ys

-- | Synonym for 'eof'. Intended usage is the @'<|>' 'orElse'@ idiom to provide
-- a fallback alternative for a chain of parsers connected with '<|>'.
--
-- > p <|> orElse x
--
-- will run parser @p@, and if @p@ consumes any token (normal or EOF), then @p@
-- will finish the parse. If @p@ doesn't consume any tokens at all and never
-- finishes then the alternative parser will produce the value @x@ upon
-- receiving EOF.
orElse :: SubstreamParser p => a -> p a
orElse = eof

-- | Convenience wrapper around 'many'. Will start with a given value of type
-- @a@, and then will parse zero or more "updates" of type @a -> a@. Updates
-- will be applied to the original value left-to-right until EOF is reached, at
-- which point the final updated value will be produced.
applyMany :: SubstreamParser p => a -> p (a -> a) -> p a
applyMany a pf = foldl (flip id) a <$> many pf

-- | Convenience wrapper around 'some'. Like 'applyMany' but will insist on
-- parsing at least one update of type @a -> a@. If no update items are parsed
-- from the input then 'applySome' will refuse to consume EOF.
applySome :: SubstreamParser p => a -> p (a -> a) -> p a
applySome a pf = foldl (flip id) a <$> some pf

-- | Like '<#>' but ignores the value produced by the right hand side parser.
(<#) :: SubstreamParser p => p a -> p b -> p a
a <# b = const <$> a <#> b

-- | Like '<#>' but ignores the value produced by the left hand side parser.
(#>) :: SubstreamParser p => p a -> p b -> p b
a #> b = (a $> id) <#> b

-- | Like '<#>' but with the types of the arguments swappped. Note that @a
-- '<##>' f@ is not the same as @f '<#>' a@. In both '<#>' and '<##>' the left
-- hand side parser looks at each input token before the right hand side
-- parser.
(<##>) :: SubstreamParser p => p a -> p (a -> b) -> p b
a <##> f = (\x y -> y x) <$> a <#> f

-- | Like '<-#>' but ignores the value produced by the right hand side parser.
(<-#) :: SubstreamParser p => p a -> p b -> p a
a <-# b = const <$> a <-#> b

-- | Like '<-#>' but ignores the value produced by the left hand side parser.
(-#>) :: SubstreamParser p => p a -> p b -> p b
a -#> b = (a $> id) <-#> b

-- | Like '<-#>' but with the types of the arguments swappped. Note that @a
-- '<-##>' f@ is not the same as @f '<#->' a@. In both cases the parser @a@
-- will be gracefully terminated once @f@ consumes an input. However, in @a
-- '<-##>' f@ it is @a@ that gets the first look at each input token, whereas in
-- @f '<#->' a@ it is @f@.
(<-##>) :: SubstreamParser p => p a -> p (a -> b) -> p b
a <-##> f = (\x y -> y x) <$> a <-#> f

-- | Like '<#->' but ignores the value produced by the right hand side parser.
(<#-) :: SubstreamParser p => p a -> p b -> p a
a <#- b = const <$> a <#-> b

-- | Like '<#->' but ignores the value produced by the left hand side parser.
(#->) :: SubstreamParser p => p a -> p b -> p b
a #-> b = (a $> id) <#-> b

-- | Like '<#->' but with the types of the arguments swappped. Note that @a
-- '<##->' f@ is not the same as @f '<-#>' a@. In both cases the parser @f@
-- will be gracefully terminated once @a@ consumes an input. However, in @a
-- '<##->' f@ it is @a@ that gets the first look at each input token, whereas in
-- @f '<-#>' a@ it is @f@.
(<##->) :: SubstreamParser p => p a -> p (a -> b) -> p b
a <##-> f = (\x y -> y x) <$> a <#-> f


-- TODO: Make wrappers to access <#> through <*> etc.

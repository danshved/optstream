{-|
Module      : Options.OptStream.Raw
Copyright   : (c) Dan Shved, 2022
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

This module contains 'RawParser' and 'RawFollower', which are the actual types
used by 'Options.OptStream.Parser' and 'Opteans.OptStream.Follower' internally.

'RawParser' is the core type of the /optstream/ library. It provides a
twice-applicative and once-monadic interface for building command line parsers.
It takes care of the parsing itself, but doesn't deal with higher-level
features such as help generation. 'Options.OptStream.Parser' is a (rather thin)
wrapper built on top of 'RawParser' in order to provide basic handling of
@--help@. You can build your own interface on top of 'RawParser' to provide
more sophisticated features.
-}
module Options.OptStream.Raw
  ( module Options.OptStream.Classes
    -- * Parsers
  , RawParser
  , runParser
  , runParserIO
  , parseArgs

    -- * Atomic parsers
  , OptionForm
  , isLegalOptionForm
    -- ** Flags
  , flag'
  , flagSep'
    -- ** Parameters
  , param'
  , paramRead'
  , paramChar'
    -- ** Free arguments
  , freeArg'
  , freeArgRead'
  , freeArgChar'
  , anyArg'
  , anyArgRead'
  , anyArgChar'
    -- ** Multi-parameters
  , multiParam'
  , RawFollower
  , next
  , nextRead
  , nextChar
  , nextMetavar

    -- * Utilities
  , withVersion'
  , withVersionIO'
  , beforeDashes

    -- * Low-level parsers
  , block
  , short
  , match
  , matchAndFollow
  , matchShort
  , quiet
  , eject

    -- * Errors
  , ParserError
  , formatParserError
  )
where

import Control.Applicative hiding (some, many)
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Foldable
import Data.Functor
import Data.List
import Data.Maybe
import Prelude hiding (fail)
import Text.Read

import Options.OptStream.Classes
import Options.OptStream.Internal
import Options.OptStream.IOOps


-- * Errors

-- At which token a DoneError occurred.
data Context
  = CtxStart
  | CtxArg String
  | CtxShort String Char
  | CtxEnd
  deriving (Eq, Ord, Show)

-- | An error returned by 'Options.OptStream.runParser'. There are three kinds of errors:
--
--   * An unexpected command line argument. This means that the top-level
--   parser skipped (didn't consume) an input token (a command-line argument or
--   a 'Options.OptStream.short' flag inside an argument).
--
--   * A missing argument. This means that either the top-level parser refused
--   to consume EOF, or that EOF was reached when a
--   'Options.OptStream.Follower' was holding the stream and wanted more input.
--   The error message will generally contain a list of possible items missing
--   (flags or metavariables).
--
--   * A custom error thrown with e.g. 'failA' or 'fmapOrFail'.
data ParserError
  -- The top-level parser didn't accept an argument.
  = UnexpectedArg String
  -- The top-level parser didn't accept a short flag.
  | UnexpectedChar Char String
  -- A Follower reached the end of input but wants more.
  | MissingArgAfter [String] String
  -- An argument is missing (a Parser refused to consume EOF).
  | MissingArg Context [String]
  -- A custom error was thrown by 'fail'.
  | CustomError Context String
  deriving (Eq, Ord, Show)

-- | Formats a 'ParserError' to a human-readable string.
formatParserError :: ParserError -> String
formatParserError (UnexpectedArg arg) =
  "unexpected command line argument " ++ show arg
formatParserError (UnexpectedChar c arg) =
  "unexpected character " ++ show c
  ++ " in command line argument " ++ show arg
formatParserError (MissingArgAfter args metavar) =
  "missing command line argument after "
  ++ (concat . intersperse " " . map show $ args)
  ++ ": " ++ metavar
formatParserError (MissingArg ctx ss) =
  "missing command line argument"
  ++ ( case ctx of
         CtxArg arg -> " before " ++ show arg
         CtxShort arg c -> " before flag " ++ show c ++ " in " ++ show arg
         CtxStart -> ""
         CtxEnd -> "" )
  ++ ": " ++ (concat . intersperse " | " $ ss)
formatParserError (CustomError ctx msg) =
  "command line error"
  ++ ( case ctx of
         CtxArg arg -> " at " ++ show arg
         CtxShort arg c -> " at flag " ++ show c ++ " in " ++ show arg
         CtxStart -> ""
         CtxEnd -> "" )
  ++ ": " ++ msg


-- * RawFollower monad

-- | A 'RawFollower' consumes zero or more strings from a stream and then
-- produces a result of type @a@. This is the type that
-- 'Options.OptStream.Follower' uses internally. The differences between
-- 'RawFollower' and 'Options.OptStream.Follower' are:
--
--   * A 'Options.OptStream.Follower' has a help string attached to it, a
--   'RawFollower' doesn't.
--
--   * 'RawFollower' is a 'Monad', whereas 'Options.OptStream.Follower' is only
--   an 'Applicative'.
data RawFollower a
  = FollowerDone (Either String a)
  | FollowerNext String (String -> RawFollower a)

data FollowerError
  = FollowerMissingArg String
  | FollowerCustomError Context String

-- | See 'Options.OptStream.nextMetavar'.
nextMetavar :: RawFollower a -> Maybe String
nextMetavar (FollowerDone _) = Nothing
nextMetavar (FollowerNext v _) = Just v

-- Left means the reader consumed all input and wants more.
runFollower :: Context
          -> RawFollower a
          -> [String]
          -> Either FollowerError (Context, a, [String])
runFollower ctx (FollowerDone (Right a)) ss = Right (ctx, a, ss)
runFollower ctx (FollowerDone (Left e)) _ = Left $ FollowerCustomError ctx e
runFollower _   (FollowerNext v _) [] = Left $ FollowerMissingArg v
runFollower _   (FollowerNext _ f) (s:ss) = runFollower (CtxArg s) (f s) ss

instance Functor RawFollower where
  fmap = liftM

instance FunctorFail RawFollower where
  fmapOrFail = fmapOrFailM

instance Applicative RawFollower where
  pure = return
  (<*>) = ap

instance ApplicativeFail RawFollower where
  failA = fail

instance Monad RawFollower where
  return = FollowerDone . Right

  FollowerDone (Right a) >>= g = g a
  FollowerDone (Left e) >>= _ = FollowerDone $ Left e
  FollowerNext v f >>= g = FollowerNext v $ (>>= g) . f

instance MonadFail RawFollower where
  fail = FollowerDone . Left

-- | See 'Options.OptStream.next'
next :: String
        -- ^ Metavariable for error messages.
     -> RawFollower String
next metavar = FollowerNext metavar return


-- * Parser monad

-- | An error that a Done parser can contain.
data DoneError
  = DEMissingArg [String]
  | DECustomError String
  deriving Show

-- | Represents a parser that has finished its job. It can be either a 'Left'
-- if the parser failed or a 'Right' if it succeeded.
type DoneParser a = Either DoneError a

-- | An EOF handler. Represents what the parser will do if the next token it
-- receives is EOF. The possibilities are:
--
--   * Refuse to consume EOF (a 'Left' value). In this case the 'Left' contains
--   a list of suggestions for which items the user could supply in order for
--   the parser to make progress.
--
--   * Consume EOF (a 'Right' value) and finish the parse. In this case the
--   'Right' contains the final state of the parser.
type EndHandler a = Either (List String) (DoneParser a)

data Action a
  = ConsumeBlock (RawFollower a)
  | ConsumeShort a

instance Functor Action where
  fmap f (ConsumeBlock fa) = ConsumeBlock $ fmap f fa
  fmap f (ConsumeShort a) = ConsumeShort $ f a

abort :: Action a -> b -> Action b
abort (ConsumeBlock _) b = ConsumeBlock $ return b
abort (ConsumeShort _) b = ConsumeShort b

type InputHandler a = Maybe String -> Maybe Char -> Maybe (Action (RawParser a))

-- | A 'RawParser' processes part of a stream of command line arguments and
-- produces an output value of type @a@. 'RawParser' is the type that
-- 'Options.OptStream.Parser' uses internally. The differences between these
-- two types are:
--
--   * A 'Options.OptStream.Parser' has a 'Options.OptStream.Help.Help' object
--   attached to it. A 'RawParser' doesn't.
--
--   * 'RawParser' is a 'Monad', whereas 'Options.OptStream.Parser' is only an
--   'Applicative'.
data RawParser a
  = Done (DoneParser a)
  | Scan (EndHandler a) (InputHandler a)


data ShortsError
  = SEUnexpectedChar Char
  | SEDoneError Context DoneError

runShorts :: String
          -> Context
          -> RawParser a
          -> [Char]
          -> Either ShortsError (Context, RawParser a)
runShorts arg = doRun where
  doRun ctx pa [] = Right (ctx, pa)
  doRun ctx (Done (Left e)) (_:_) = Left $ SEDoneError ctx e
  doRun _   (Done (Right _)) (c:_) = Left $ SEUnexpectedChar c
  doRun _   (Scan _ inputH) (c:cs) = case inputH Nothing (Just c) of
    Just (ConsumeShort pa') -> doRun (CtxShort arg c) pa' cs
    Just (ConsumeBlock _) -> error "ConsumeBlock in response to short input"
    Nothing -> Left $ SEUnexpectedChar c

missingArg :: Context -> List String -> ParserError
missingArg ctx = MissingArg ctx . nubOrd . toList

doneMissingArg :: List String -> DoneParser a
doneMissingArg = Left . DEMissingArg . nubOrd . toList

toParserError :: Context -> DoneError -> ParserError
toParserError ctx (DEMissingArg vs) = MissingArg ctx vs
toParserError ctx (DECustomError msg) = CustomError ctx msg

-- | See 'Options.OptStream.runParser'.
runParser :: RawParser a -> [String] -> Either ParserError a
runParser = doRun CtxStart where
  doRun ctx (Done (Left e)) _ = Left $ toParserError ctx e
  doRun _   (Done (Right a)) [] = Right $ a
  doRun _   (Done (Right _)) (s:_) = Left $ UnexpectedArg s

  doRun _   (Scan (Left xs) _) [] = Left $ missingArg CtxEnd xs
  doRun _   (Scan (Right (Right a)) _) [] = Right a
  doRun _   (Scan (Right (Left e)) _) [] = Left $ toParserError CtxEnd e

  doRun ctx pa@(Scan _ inputH) (s:ss) = case inputH (Just s) mc of
    Just (ConsumeBlock fpa) -> case runFollower (CtxArg s) fpa ss of
      Right (ctx', pa', ss') -> doRun ctx' pa' ss'
      Left (FollowerMissingArg v) -> Left $ MissingArgAfter (s:ss) v
      Left (FollowerCustomError ctx' e) -> Left $ CustomError ctx' e
    Just (ConsumeShort pa') -> case shorts of
      Just (c, cs) -> case runShorts s (CtxShort s c) pa' cs of
        Right (ctx', pa'') -> doRun ctx' pa'' ss
        Left (SEUnexpectedChar c') -> Left $ UnexpectedChar c' s
        Left (SEDoneError ctx' e) -> Left $ toParserError ctx' e
      Nothing -> error "ConsumeShort in response to long input"
    Nothing -> Left $ UnexpectedArg s
    where
      shorts = case s of
        ('-':(c:cs)) -> Just (c, cs)
        _ -> Nothing
      mc = fmap fst shorts


-- ** Instances

endAlternative :: EndHandler a -> EndHandler a -> EndHandler a
endAlternative (Right da) _ = Right da
endAlternative _ (Right da) = Right da
endAlternative (Left xs) (Left xs') = Left $ xs <> xs'

endParallel :: EndHandler (a -> b) -> EndHandler a -> EndHandler b
endParallel (Right (Left e)) _ = Right (Left e)
endParallel (Right (Right f)) eda = (fmap . fmap) f eda
endParallel _ (Right (Left e)) = Right (Left e)
endParallel edf (Right (Right a)) = (fmap . fmap) ($ a) edf
endParallel (Left xs) (Left xs') = Left $ xs <> xs'

instance Functor RawParser where
  fmap = liftM

instance FunctorFail RawParser where
  fmapOrFail = fmapOrFailM

instance Applicative RawParser where
  pure = return
  (<*>) = ap

instance ApplicativeFail RawParser where
  failA = fail

instance Monad RawParser where
  return = Done . Right

  Done (Right a) >>= f = f a
  Done (Left e) >>= _ = Done $ Left e
  Scan endH inputH >>= f = Scan endH' inputH' where
    endH' = case endH of
      Left xs -> Left xs
      Right (Left e) -> Right (Left e)
      Right (Right a) -> case f a of
        Done db -> Right db
        Scan endH'' _ -> endH''
    inputH' ms mc = (fmap . fmap) (>>= f) $ inputH ms mc

instance MonadFail RawParser where
  fail = Done . Left . DECustomError

instance Alternative RawParser where
  empty = Scan (Left mempty) (const $ const Nothing)

  Done da <|> _ = Done da
  _ <|> Done da = Done da
  Scan endH inputH <|> Scan endH' inputH' =
    Scan endH'' inputH'' where
      endH'' = endH `endAlternative` endH'
      inputH'' ms mc = inputH ms mc <|> inputH' ms mc

instance SelectiveParser RawParser where
  Done (Right f) <#> pa = fmap f pa
  Done (Left e) <#> _ = Done $ Left e
  pf <#> Done (Right a) = fmap ($ a) pf
  _ <#> Done (Left e) = Done $ Left e
  pf@(Scan endH inputH) <#> pa@(Scan endH' inputH') =
    Scan endH'' inputH'' where
      endH'' = endH `endParallel` endH'
      inputH'' ms mc = case inputH ms mc of
        Just apf -> Just $ fmap (<#> pa) apf
        Nothing -> (fmap . fmap) (pf <#>) $ inputH' ms mc

  Done (Right f) <-#> pa = fmap f pa
  Done (Left e) <-#> _ = Done $ Left e
  Scan (Right df) _ <-#> Done da = Done $ df <*> da
  Scan (Left xs) _ <-#> Done (Right _) = Done $ doneMissingArg xs
  Scan (Left _) _ <-#> Done (Left e) = Done $ Left e
  Scan endH inputH <-#> pa@(Scan endH' inputH') = Scan endH'' inputH'' where
    endH'' = endH `endParallel` endH'
    inputH'' ms mc = case inputH ms mc of
      Just apf -> Just $ fmap (<-#> pa) apf
      Nothing -> case inputH' ms mc of
        Just apa -> case endH of
          Right (Right f) -> Just $ (fmap . fmap) f apa
          Right (Left e) -> Just . abort apa . Done $  Left e
          Left xs -> Just . abort apa . Done $ doneMissingArg xs
        Nothing -> Nothing

  Done df <#-> Done da = Done $ df <*> da
  Done df <#-> Scan (Right da) _ = Done $ df <*> da
  Done (Right _) <#-> Scan (Left xs) _ = Done $ doneMissingArg xs
  Done (Left e) <#-> Scan (Left _) _ = Done $ Left e
  pf <#-> Done (Right a) = fmap ($ a) pf
  _ <#-> Done (Left e) = Done $ Left e
  pf@(Scan endH inputH) <#-> Scan endH' inputH' = Scan endH'' inputH'' where
    endH'' =  endH `endParallel` endH'
    inputH'' ms mc = case inputH ms mc of
      Just apf -> case endH' of
        Right (Right a) -> Just $ (fmap . fmap) ($ a) apf
        Right (Left e) -> Just . abort apf . Done $ Left e
        Left xs -> Just . abort apf . Done $ doneMissingArg xs
      Nothing -> (fmap . fmap) (pf <#->) $ inputH' ms mc

  Done da <-|> _ = Done da
  Scan _ _ <-|> Done da = Done da
  Scan endH inputH <-|> r@(Scan endH' inputH') = Scan endH'' inputH'' where
    endH'' = endH `endAlternative` endH'
    inputH'' ms mc = case inputH ms mc of
      Just apa -> Just $ fmap (<-|> r) apa
      Nothing -> inputH' ms mc

  Done da <|-> _ = Done da
  Scan _ _ <|-> Done da = Done da
  l@(Scan endH inputH) <|-> Scan endH' inputH' = Scan endH'' inputH''  where
    endH'' = endH `endAlternative` endH'
    inputH'' ms mc = case inputH ms mc of
      Just apa -> Just apa
      Nothing -> (fmap . fmap) (l <|->) $ inputH' ms mc

  eof = Scan (Right $ Right ()) (const $ const Nothing)



-- ** Primitive parsers

-- | See 'Options.OptStream.block'.
block :: String
         -- ^ Block name for "missing argument" error messages. Arbitrary
         -- string.
      -> (String -> Maybe (RawFollower a))
         -- ^ A function that decides whether to skip or consume a command line
         -- argument.
      -> RawParser a
         -- ^ A 'RawParser' that consumes one consecutive block of command line
         -- arguments.
block name f = Scan endH inputH where
  endH = Left $ single name
  inputH (Just s) _ = fmap (ConsumeBlock . fmap return) $ f s
  inputH _ _ = Nothing

-- TODO: don't consume the long version once runParser is updated.
-- | See 'Options.OptStream.short'.
short :: String
         -- ^ Short flag name for "missing argument" error messages. Arbitrary
         -- string.
      -> (Char -> Maybe a)
         -- ^ A function that decides whether to skip or consume a short flag.
      -> RawParser a
         -- ^ A 'RawParser' that consumes one short flag.
short name f = Scan endH inputH where
  endH = Left $ single name
  inputH _ (Just c) = fmap (ConsumeShort . return) $ f c
  inputH _ _ = Nothing

-- | See 'Options.OptStream.quiet'.
quiet :: RawParser a -> RawParser a
quiet (Scan (Left _) inputH) = Scan (Left mempty) inputH
quiet x = x


-- ** Matchers

-- | See 'Options.OptStream.match'.
match :: String
         -- ^ The exact command line argument to match.
      -> RawParser String
         -- ^ A parser that finishes after matching and consuming the argument.
match s = matchAndFollow s $ return s

-- | See 'Options.OptStream.matchAndFollow'.
matchAndFollow :: String
                  -- ^ Command line argument that starts a block.
               -> RawFollower a
                  -- ^ A follower that consumes the rest of the block.
               -> RawParser a
matchAndFollow s fa = block s $ \arg -> guard (arg == s) $> fa

-- | See 'Options.OptStream.matchShort'.
matchShort :: Char
              -- ^ A short flag, e.g. @\'x\'@ will match @-x@ or an occurence
              -- of @\'x\'@ in a bundle of short flags like @-xyz@.
           -> RawParser Char
matchShort c = short ['-', c] $ \c' -> guard (c' == c) $> c'

dropAll :: RawParser ()
dropAll = (void (anyArg' "") <|> void (anyShort' "")) *> dropAll <|> orElse ()


-- ** Parsers for parameter values

parseRead :: Read a => String -> Either String a
parseRead = readEither

parseChar :: String -> Either String Char
parseChar [c] = Right c
parseChar [] = Left "expected one character, got zero"
parseChar s = Left $ "expected one character, got " ++ show (length s)

-- ** High level matchers


-- *** Flag

flag1 :: Option -> RawParser ()
flag1 (Short c) = void $ matchShort c
flag1 (Long s) = void . match $ "--" ++ s

-- | See 'Options.OptStream.flag''.
flag' :: [OptionForm]
         -- ^ Flag forms, e.g. @["-f", "--foo"]@.
      -> RawParser ()
         -- ^ A parser that succeeds upon consuming the flag.
flag' [] = error "empty list of option strings"
flag' ss = asum $ map (flag1 . parseOptionForm) ss

flagSep1 :: Option -> RawParser ()
flagSep1 (Short c) = void $ match ['-', c]
flagSep1 (Long s) = void . match $ "--" ++ s

-- | See 'Options.OptStream.flagSep''.
flagSep' :: [OptionForm]
            -- ^ Flag forms, e.g. @["-f", "--foo"]@.
         -> RawParser ()
            -- ^ A parser that succeeds upon consuming the flag.
flagSep' [] = error "empty list of option strings"
flagSep' ss = asum $ map (flagSep1 . parseOptionForm) ss


-- *** Param

cutPrefix :: String -> String -> Maybe String
cutPrefix a b
  | a `isPrefixOf` b = Just $ drop (length a) b
  | otherwise = Nothing

cutProperPrefix :: String -> String -> Maybe String
cutProperPrefix a b
  | a `isPrefixOf` b && la < lb = Just $ drop la b
  | otherwise = Nothing
  where
    la = length a
    lb = length b

param1 :: Option -> String -> RawParser String
param1 (Short c) metavar
  =   block
        prefix
        (\arg -> guard (arg == prefix) $> next metavar)
  <|> quiet ( block
        (prefix ++ metavar)
        (fmap return . cutProperPrefix prefix)
      )
  where prefix = ['-', c]
param1 (Long s) metavar
  =   block
        prefix
        (\arg -> guard (arg == prefix) $> next metavar)
  <|> quiet ( block
        (prefix ++ "=" ++ metavar)
        (fmap return . cutPrefix (prefix ++ "="))
      )
  where prefix = "--" ++ s

-- | See 'Options.OptStream.param''.
param' :: [OptionForm]
          -- ^ All parameter forms, e.g. @["-n", "--name"]@.
       -> String
          -- ^ Metavariable for error messages.
       -> RawParser String
          -- ^ A parser that returns the parameter value.
param' [] _ = error "empty list of option strings"
param' opts metavar = asum $ map f opts where
  f opt = param1 (parseOptionForm opt) metavar

-- | See 'Options.OptStream.paramRead''.
paramRead' :: Read a
           => [OptionForm]
              -- ^ All parameter forms, e.g. @["-n", "--number"]@.
           -> String
              -- ^ Metavariable for error messages.
           -> RawParser a
              -- ^ A parser that returns the parsed parameter value.
paramRead' opts metavar = parseRead <$?> param' opts metavar

-- | See 'Options.OptStream.paramChar''.
paramChar' :: [OptionForm]
              -- ^ All parameter forms, e.g. @["-s", "--separator"]@.
           -> String
              -- ^ Metavariable for error messages.
           -> RawParser Char
              -- ^ A parser that returns the parsed parameter value.
paramChar' opts metavar = parseChar <$?> param' opts metavar


-- *** Free arguments

isFreeArg :: String -> Bool
isFreeArg ('-':_) = False
isFreeArg _ = True

-- | See 'Options.OptStream.freeArg''.
freeArg' :: String
            -- ^ Metavariable for error messages (arbitrary string).
         -> RawParser String
            -- ^ Parser that consumes and returns the first free argument it
            -- sees.
freeArg' metavar = block metavar $ \arg -> guard (isFreeArg arg) $> return arg

-- | See 'Options.OptStream.freeArgRead''.
freeArgRead' :: Read a
             => String
                -- ^ Metavariable for error messages (arbitrary string).
             -> RawParser a
                -- ^ Parser that consumes the first free argument it sees and
                -- parses it down to type @a@.
freeArgRead' metavar = parseRead <$?> freeArg' metavar

-- | See 'Options.OptStream.freeArgChar''.
freeArgChar' :: String
                -- ^ Metavariable for error messages.
             -> RawParser Char
                -- ^ Parser that consumes the first free argument it sees and
                -- parses it down to a 'Data.Char.Char'.
freeArgChar' metavar = parseChar <$?> freeArg' metavar

-- | See 'Options.OptStream.anyArg''.
anyArg' :: String
           -- ^ Metavariable for error messages.
        -> RawParser String
           -- ^ Parser that consumes and returns the first argument it sees.
anyArg' metavar = block metavar (Just . return)

-- | See 'Options.OptStream.anyArgRead''.
anyArgRead' :: Read a
            => String
               -- ^ Metavariable for error messages.
            -> RawParser a
               -- ^ Parser that consumes the first argument it sees and parses
               -- it down to type @a@.
anyArgRead' metavar = parseRead <$?> anyArg' metavar

-- | See 'Options.OptStream.anyArgChar''.
anyArgChar' :: String
               -- ^ Metavariable for error messages.
            -> RawParser Char
               -- ^ Parser that consumes the first argument it sees and parses
               -- it down to a 'Char'.
anyArgChar' metavar = parseChar <$?> anyArg' metavar

-- | Consumes any short flag. Not exported for now as usage is unclear.
anyShort' :: String
             -- ^ Metavariable for error messages.
          -> RawParser Char
             -- ^ Parser that consumes and returns the first short flag it
             -- sees.
anyShort' metavar = short metavar Just

-- *** Multi-parameters

multiParam1 :: Option -> RawFollower a -> RawParser a
multiParam1 (Short c) = matchAndFollow ['-', c]
multiParam1 (Long s) = matchAndFollow ("--" ++ s)

-- | See 'Options.OptStream.multiParam''.
multiParam' :: [OptionForm]
              -- ^ All multi-parameter forms, e.g. @["-p", "--person"]@.
            -> RawFollower a
              -- ^ How to process the following arguments.
            -> RawParser a
              -- ^ A parser that consumes the option form and the following
              -- arguments.
multiParam' [] _ = error "empty list of option strings"
multiParam' opts ra = asum $ map f opts where
  f opt = multiParam1 (parseOptionForm opt) ra

-- | See 'Options.OptStream.nextRead'.
nextRead :: Read a
         => String
            -- ^ Metavariable for error messages.
         -> RawFollower a
nextRead v = parseRead <$?> next v

-- | See 'Options.OptStream.nextChar'.
nextChar :: String
            -- ^ Metavariable for error messages.
         -> RawFollower Char
nextChar v = parseChar <$?> next v


-- ** Utilities

-- | See 'Options.OptStream.eject'.
eject :: RawParser a
         -- ^ An existing parser.
      -> RawParser b
         -- ^ A parser that may trigger an ejection.
      -> RawParser (Either b a)
eject a b = (Right <$> a <* eof) <-|> quiet (Left <$> b <* dropAll)

-- | See 'Options.OptStream.withVersion''.
withVersion' :: String
                -- ^ Version info to be shown to the user.
             -> RawParser a
                -- ^ An existing 'RawParser'.
             -> RawParser (Either String a)
                -- ^ A wrapper 'RawParser' that returns either @a@ or the given
                -- version string.
withVersion' s pa = eject pa $ flag' ["--version"] $> s

-- | See 'Options.OptStream.beforeDashes'.
beforeDashes :: RawParser a
                -- ^ An existing 'RawParser'.
             -> RawParser a
                -- ^ A wrapper that handles @--@.
beforeDashes pa =  pa <-# (void (match "--") <|> orElse ())


-- ** IO helpers

-- | See 'Options.OptStream.runParserIO'.
runParserIO :: IOOps m => RawParser a -> [String] -> m a
runParserIO pa args = case runParser pa args of
  Right a -> return a
  Left e -> do
    name <- getProgName
    die $ name ++ ": " ++ formatParserError e

-- | See 'Options.OptStream.parseArgs'.
parseArgs :: IOOps m => RawParser a -> m a
parseArgs pa = getArgs >>= runParserIO pa

-- | See 'Options.OptStream.withVersionIO''.
withVersionIO' :: IOOps m
               => String
                  -- ^ Version information to show to the user.
               -> RawParser (m a)
                  -- ^ An existing 'RawParser'.
               -> RawParser (m a)
                  -- ^ A wrapper that handles @--version@.
withVersionIO' s = fmap (join . versionToIO) . withVersion' s


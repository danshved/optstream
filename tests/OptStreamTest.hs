module Main where

import Data.Either
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Options.OptStream

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "flag'"
    [ testProperty "Matches"       prop_flag'_Matches
    , testProperty "Finishes"      prop_flag'_Finishes
    , testProperty "Empty"         prop_flag'_Empty
    , testProperty "NotMatches"    prop_flag'_NotMatches
    , testProperty "Skips"         prop_flag'_Skips
    , testProperty "MatchesBundle" prop_flag'_MatchesBundle
    ]

  , testGroup "flagSep'"
    [ testProperty "Matches"          prop_flagSep'_Matches
    , testProperty "Finishes"         prop_flagSep'_Finishes
    , testProperty "Empty"            prop_flagSep'_Empty
    , testProperty "NotMatches"       prop_flagSep'_NotMatches
    , testProperty "Skips"            prop_flagSep'_Skips
    , testProperty "NotMatchesBundle" prop_flagSep'_NotMatchesBundle
    ]

  , testGroup "param'"
    [ testProperty "Matches"       prop_param'_Matches
    , testProperty "MatchesShort"  prop_param'_MatchesShort
    , testProperty "MatchesLong"   prop_param'_MatchesLong
    , testProperty "Finishes"      prop_param'_Finishes
    , testProperty "FinishesShort" prop_param'_FinishesShort
    , testProperty "FinishesLong"  prop_param'_FinishesLong
    , testProperty "Empty"         prop_param'_Empty
    , testProperty "MissingArg"    prop_param'_MissingArg
    , testProperty "NotMatches"    prop_param'_NotMatches
    , testProperty "Skips"         prop_param'_Skips
    ]

  , testGroup "freeArg'"
    [ testProperty "Matches"    prop_freeArg_Matches
    , testProperty "Finishes"   prop_freeArg_Finishes
    , testProperty "Empty"      prop_freeArg_Empty
    , testProperty "NotMatches" prop_freeArg_NotMatches
    , testProperty "Skips"      prop_freeArg_Skips
    ]

  , testGroup "multiParam'"
    [ testProperty "Matches"    prop_multiParam'_Matches
    , testProperty "Finishes"   prop_multiParam'_Finishes
    , testProperty "Empty"      prop_multiParam'_Empty
    , testProperty "NotEnough"  prop_multiParam'_NotEnough
    , testProperty "NotMatches" prop_multiParam'_NotMatches
    , testProperty "Skips"      prop_multiParam'_Skips
    ]
  ]

-- | Represents an arbitrary legal option form.
newtype Legal = Legal { unLegal :: OptionForm }
  deriving (Eq, Ord, Show)

instance Arbitrary Legal where
  arbitrary = oneof
    [ do c <- arbitrary `suchThat` (/= '-')
         return $ Legal ['-', c]
    , do s <- arbitrary `suchThat` (/= "")
         return $ Legal ('-':'-':s)
    ]

  shrink (Legal ('-':'-':s)) = [Legal ('-':'-':s') | s' <- shrink s, s' /= ""]
    ++ [Legal ['-', c] | c <- "abc"]
  shrink (Legal ['-', c]) = [Legal ['-', c'] | c' <- shrink c, c' /= '-']
  shrink _ = []

-- | Represents an arbitrary list of legal option forms (possibly empty).
newtype Legals = Legals { unLegals :: [OptionForm] }
  deriving (Eq, Ord, Show)

instance Arbitrary Legals where
  arbitrary = Legals . map unLegal <$> arbitrary
  shrink (Legals ss) = map Legals $ shrinkList (map unLegal . shrink . Legal) ss

-- | Represents an arbitrary character other than '-'.
newtype NotDash = NotDash { unNotDash :: Char }
  deriving (Eq, Ord, Show)

instance Arbitrary NotDash where
  arbitrary = NotDash <$> arbitrary `suchThat` (/= '-')
  shrink (NotDash c) = [NotDash c' | c' <- shrink c, c' /= '-']

-- | Represents an arbitrary free argument.
newtype Free = Free { unFree :: String }
  deriving (Eq, Ord, Show)

instance Arbitrary Free where
  arbitrary = Free <$> arbitrary `suchThat` isFree
  shrink (Free s) = [Free s' | s' <- shrink s, isFree s']

isFree :: String -> Bool
isFree ('-':_) = False
isFree _ = True

-- Helper parser that collects all the arguments it gets.
args :: Parser [String]
args = many (anyArg' "ARG")


prop_flag'_Matches (Legals as) (Legal b) (Legals cs) =
  runParser (flag' forms) [b] == Right ()
  where
    forms = as ++ [b] ++ cs

prop_flag'_Finishes (Legals as) (Legal b) (Legals cs) ds =
  runParser (flag' forms *> args) (b:ds) == Right ds
  where
    forms = as ++ [b] ++ cs

prop_flag'_Skips (Legals as) (Legal b) (Legals cs) d =
  not (d `elem` forms) ==>
  runParser (flag' forms #> args) [d, b] == Right [d]
  where
    forms = as ++ [b] ++ cs

prop_flag'_Empty (Legal a) (Legals bs) =
  isLeft $ runParser (flag' forms) []
  where
    forms = a:bs

prop_flag'_NotMatches (Legal a) (Legals bs) c =
  not (c `elem` forms) ==>
  isLeft $ runParser (flag' forms) [c]
  where
    forms = a:bs

prop_flag'_MatchesBundle cs =
  length cs /= 0 ==>
  runParser p ['-':map unNotDash cs] == Right [() | c <- cs]
  where
    p = sequenceA [flag' [['-', c]] | NotDash c <- cs]


prop_flagSep'_Matches (Legals as) (Legal b) (Legals cs) =
  runParser (flagSep' forms) [b] == Right ()
  where
    forms = as ++ [b] ++ cs

prop_flagSep'_Finishes (Legals as) (Legal b) (Legals cs) ds =
  runParser (flagSep' forms *> args) (b:ds) == Right ds
  where
    forms = as ++ [b] ++ cs

prop_flagSep'_Skips (Legals as) (Legal b) (Legals cs) d =
  not (d `elem` forms) ==>
  runParser (flagSep' forms #> args) [d, b] == Right [d]
  where
    forms = as ++ [b] ++ cs

prop_flagSep'_Empty (Legal a) (Legals bs) =
  isLeft $ runParser (flagSep' forms) []
  where
    forms = a:bs

prop_flagSep'_NotMatches (Legal a) (Legals bs) c =
  not (c `elem` forms) ==>
  isLeft $ runParser (flagSep' forms) [c]
  where
    forms = a:bs

prop_flagSep'_NotMatchesBundle cs =
  length cs >= 2 ==>
  isLeft $ runParser p ['-':map unNotDash cs]
  where
    p = sequenceA [flagSep' [['-', c]] | NotDash c <- cs]


prop_param'_Matches (Legals as) (Legal b) (Legals cs) meta d =
  runParser (param' forms meta) [b, d] == Right d
  where
    forms = as ++ [b] ++ cs

prop_param'_MatchesShort (Legals as) (NotDash b) (Legals cs) meta (NonEmpty d) =
  runParser (param' forms meta) ['-':b:d] == Right d
  where
    forms = as ++ [['-', b]] ++ cs

prop_param'_MatchesLong (Legals as) (NonEmpty b) (Legals cs) meta d =
  runParser (param' forms meta) ["--" ++ b ++ "=" ++ d] == Right d
  where
    forms = as ++ ["--" ++ b] ++ cs

prop_param'_Finishes (Legals as) (Legal b) (Legals cs) meta d es =
  runParser (param' forms meta *> args) (b:d:es) == Right es
  where
    forms = as ++ [b] ++ cs

prop_param'_FinishesShort
  (Legals as) (NotDash b) (Legals cs) meta (NonEmpty d) es =
  runParser (param' forms meta *> args) (('-':b:d):es) == Right es
  where
    forms = as ++ [['-', b]] ++ cs

prop_param'_FinishesLong (Legals as) (NonEmpty b) (Legals cs) meta d es =
  runParser (param' forms meta *> args) (assign:es) == Right es
  where
    forms = as ++ ["--" ++ b] ++ cs
    assign = "--" ++ b ++ "=" ++ d

prop_param'_Skips (Legals as) (Legal b) (Legals cs) meta d e =
  not (any (`isPrefixOf` e) forms) ==>
  runParser (param' forms meta #> args) [e, b, d] == Right [e]
  where
    forms = as ++ [b] ++ cs

prop_param'_Empty (Legal a) (Legals bs) meta =
  isLeft $ runParser (param' forms meta) []
  where
    forms = a:bs

prop_param'_MissingArg (Legals as) (Legal b) (Legals cs) meta =
  isLeft $ runParser (param' forms meta) [b]
  where
    forms = as ++ [b] ++ cs

prop_param'_NotMatches (Legal a) (Legals bs) meta c d =
  not (any (`isPrefixOf` c) forms) ==>
  isLeft $ runParser (param' forms meta *> args) [c, d]
  where
    forms = a:bs


data FreeArgMaker
  = FreeArg' String
  | FreeArg String String
  deriving (Eq, Ord, Show)

toFreeArg :: FreeArgMaker -> Parser String
toFreeArg (FreeArg' meta) = freeArg' meta
toFreeArg (FreeArg meta desc) = freeArg meta desc

instance Arbitrary FreeArgMaker where
  arbitrary = oneof
    [ FreeArg' <$> arbitrary
    , FreeArg <$> arbitrary <*> arbitrary
    ]

  shrink (FreeArg' meta) = [FreeArg' meta' | meta' <- shrink meta]
  shrink (FreeArg meta desc) = [FreeArg' meta] ++
    [FreeArg meta desc' | desc' <- shrink desc] ++
    [FreeArg meta' desc | meta' <- shrink meta]

prop_freeArg_Matches maker (Free a) =
  runParser (toFreeArg maker) [a] == Right a

prop_freeArg_Finishes maker (Free a) bs =
  runParser (toFreeArg maker *> args) (a:bs) == Right bs

prop_freeArg_Skips maker (Free a) b =
  runParser (toFreeArg maker #> args) ['-':b, a] == Right ['-':b]

prop_freeArg_Empty maker =
  isLeft $ runParser (toFreeArg maker) []

prop_freeArg_NotMatches maker a =
  isLeft $ runParser (toFreeArg maker) ['-':a]


prop_multiParam'_Matches (Legals as) (Legal b) (Legals cs) dms =
  runParser (multiParam' forms $ traverse next ms) (b:ds) == Right ds
  where
    forms = as ++ [b] ++ cs
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam'_Finishes (Legals as) (Legal b) (Legals cs) dms es =
  runParser (multiParam' forms f *> args) ([b] ++ ds ++ es) == Right es
  where
    forms = as ++ [b] ++ cs
    f = traverse next ms
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam'_Skips (Legals as) (Legal b) (Legals cs) dms e =
  not (e `elem` forms) ==>
  runParser (multiParam' forms f #> args) (e:b:ds) == Right [e]
  where
    forms = as ++ [b] ++ cs
    f = traverse next ms
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam'_Empty (Legal a) (Legals bs) ms =
  isLeft $ runParser (multiParam' forms f) []
  where
    forms = a:bs
    f :: Follower [String]
    f = traverse next ms

prop_multiParam'_NotEnough (Legals as) (Legal b) (Legals cs) dms (NonEmpty ms) =
  isLeft $ runParser (multiParam' forms $ traverse next ms') (b:ds)
  where
    forms = as ++ [b] ++ cs
    ms' = map snd dms ++ ms  -- Metavariables.
    ds = map fst dms         -- Arguments that should match part of them.

prop_multiParam'_NotMatches (Legal a) (Legals bs) ms c cs =
  not (c `elem` forms) ==>
  isLeft $ runParser (multiParam' forms f *> args) (c:cs)
  where
    forms = a:bs
    f :: Follower [String]
    f = traverse next ms


-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

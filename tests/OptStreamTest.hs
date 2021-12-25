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
    [ testProperty "Matches"       prop_flag_Matches
    , testProperty "Finishes"      prop_flag_Finishes
    , testProperty "Empty"         prop_flag_Empty
    , testProperty "NotMatches"    prop_flag_NotMatches
    , testProperty "Skips"         prop_flag_Skips
    , testProperty "MatchesBundle" prop_flag_MatchesBundle
    ]

  , testGroup "flagSep'"
    [ testProperty "Matches"          prop_flagSep_Matches
    , testProperty "Finishes"         prop_flagSep_Finishes
    , testProperty "Empty"            prop_flagSep_Empty
    , testProperty "NotMatches"       prop_flagSep_NotMatches
    , testProperty "Skips"            prop_flagSep_Skips
    , testProperty "NotMatchesBundle" prop_flagSep_NotMatchesBundle
    ]

  , testGroup "param'"
    [ testProperty "Matches"       prop_param_Matches
    , testProperty "MatchesShort"  prop_param_MatchesShort
    , testProperty "MatchesLong"   prop_param_MatchesLong
    , testProperty "Finishes"      prop_param_Finishes
    , testProperty "FinishesShort" prop_param_FinishesShort
    , testProperty "FinishesLong"  prop_param_FinishesLong
    , testProperty "Empty"         prop_param_Empty
    , testProperty "MissingArg"    prop_param_MissingArg
    , testProperty "NotMatches"    prop_param_NotMatches
    , testProperty "Skips"         prop_param_Skips
    ]

  , testGroup "freeArg'"
    [ testProperty "Matches"    prop_freeArg_Matches
    , testProperty "Finishes"   prop_freeArg_Finishes
    , testProperty "Empty"      prop_freeArg_Empty
    , testProperty "NotMatches" prop_freeArg_NotMatches
    , testProperty "Skips"      prop_freeArg_Skips
    ]

  , testGroup "multiParam'"
    [ testProperty "Matches"    prop_multiParam_Matches
    , testProperty "Finishes"   prop_multiParam_Finishes
    , testProperty "Empty"      prop_multiParam_Empty
    , testProperty "NotEnough"  prop_multiParam_NotEnough
    , testProperty "NotMatches" prop_multiParam_NotMatches
    , testProperty "Skips"      prop_multiParam_Skips
    ]
  ]

-- | Represents an arbitrary legal option form.
newtype Legal = Legal { unLegal :: OptionForm }
  deriving Show

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
  deriving Show

instance Arbitrary Legals where
  arbitrary = Legals . map unLegal <$> arbitrary
  shrink (Legals ss) = map Legals $ shrinkList (map unLegal . shrink . Legal) ss

-- | Represents a set of legal option forms with one of them selected.
data Forms = Forms Legals Legal Legals
  deriving Show

allForms :: Forms -> [OptionForm]
allForms (Forms (Legals as) (Legal b) (Legals cs)) = as ++ [b] ++ cs

chosenForm :: Forms -> OptionForm
chosenForm (Forms _ (Legal x) _) = x

instance Arbitrary Forms where
  arbitrary = Forms <$> arbitrary <*> arbitrary <*> arbitrary

  shrink (Forms x y z) =
    [Forms x' y z | x' <- shrink x] ++
    [Forms x y' z | y' <- shrink y] ++
    [Forms x y z' | z' <- shrink z]

-- | Represents an arbitrary character other than '-'.
newtype NotDash = NotDash { unNotDash :: Char }
  deriving Show

instance Arbitrary NotDash where
  arbitrary = NotDash <$> arbitrary `suchThat` (/= '-')
  shrink (NotDash c) = [NotDash c' | c' <- shrink c, c' /= '-']

-- | Represents an arbitrary free argument.
newtype Free = Free { unFree :: String }
  deriving Show

instance Arbitrary Free where
  arbitrary = Free <$> arbitrary `suchThat` isFree
  shrink (Free s) = [Free s' | s' <- shrink s, isFree s']

isFree :: String -> Bool
isFree ('-':_) = False
isFree _ = True

-- Helper parser that collects all the arguments that it gets.
args :: Parser [String]
args = many (anyArg' "ARG")


-- | Represents a choice between 'flag' and 'flag''.
data FlagMaker
  = Flag'
  | Flag String
  deriving Show

mkFlag :: FlagMaker -> [OptionForm] -> Parser ()
mkFlag Flag' opts = flag' opts
mkFlag (Flag desc) opts = flag opts desc

mkFlagSep :: FlagMaker -> [OptionForm] -> Parser ()
mkFlagSep Flag' opts = flagSep' opts
mkFlagSep (Flag desc) opts = flagSep opts desc

instance Arbitrary FlagMaker where
  arbitrary = oneof [pure Flag', Flag <$> arbitrary]

  shrink Flag' = []
  shrink (Flag s) = Flag':[Flag s' | s' <- shrink s]


prop_flag_Matches maker fs =
  runParser (mkFlag maker $ allForms fs) [chosenForm fs] == Right ()

prop_flag_Finishes maker fs xs =
  runParser (mkFlag maker (allForms fs) *> args) (chosenForm fs:xs) == Right xs

prop_flag_Skips maker fs x =
  not (x `elem` forms) ==>
  runParser (mkFlag maker forms #> args) [x, chosenForm fs] == Right [x]
  where
    forms = allForms fs

prop_flag_Empty maker fs =
  isLeft $ runParser (mkFlag maker $ allForms fs) []

prop_flag_NotMatches maker fs x =
  not (x `elem` forms) ==>
  isLeft $ runParser (mkFlag maker forms) [x]
  where
    forms = allForms fs

prop_flag_MatchesBundle maker (NonEmpty cs) =
  runParser p ['-':chars] == Right [() | c <- chars]
  where
    p = sequenceA [mkFlag maker [['-', c]] | c <- chars]
    chars = [c | NotDash c <- cs]


prop_flagSep_Matches maker fs =
  runParser (mkFlagSep maker $ allForms fs) [chosenForm fs] == Right ()

prop_flagSep_Finishes maker fs xs =
  runParser (mkFlagSep maker (allForms fs) *> args) (chosenForm fs:xs)
    == Right xs

prop_flagSep_Skips maker fs x =
  not (x `elem` forms) ==>
  runParser (mkFlagSep maker forms #> args) [x, chosenForm fs] == Right [x]
  where
    forms = allForms fs

prop_flagSep_Empty maker fs =
  isLeft $ runParser (mkFlagSep maker $ allForms fs) []

prop_flagSep_NotMatches maker fs x =
  not (x `elem` forms) ==>
  isLeft $ runParser (mkFlagSep maker forms) [x]
  where
    forms = allForms fs

prop_flagSep_NotMatchesBundle maker cs =
  length cs >= 2 ==>
  isLeft $ runParser p ['-':map unNotDash cs]
  where
    p = sequenceA [mkFlagSep maker [['-', c]] | NotDash c <- cs]


data ParamMaker
  = Param' String
  | Param String String
  deriving Show

mkParam :: ParamMaker -> [OptionForm] -> Parser String
mkParam (Param' metavar) opts = param' opts metavar
mkParam (Param metavar desc) opts = param opts metavar desc

instance Arbitrary ParamMaker where
  arbitrary = oneof
    [ Param' <$> arbitrary
    , Param <$> arbitrary <*> arbitrary
    ]

  shrink (Param' metavar) = [Param' metavar' | metavar' <- shrink metavar]
  shrink (Param metavar desc) = [Param' metavar] ++
    [Param metavar desc' | desc' <- shrink desc] ++
    [Param metavar' desc | metavar' <- shrink metavar]

prop_param_Matches maker (Legals as) (Legal b) (Legals cs) d =
  runParser (mkParam maker forms) [b, d] == Right d
  where
    forms = as ++ [b] ++ cs

prop_param_MatchesShort maker (Legals as) (NotDash b) (Legals cs) (NonEmpty d) =
  runParser (mkParam maker forms) ['-':b:d] == Right d
  where
    forms = as ++ [['-', b]] ++ cs

prop_param_MatchesLong maker (Legals as) (NonEmpty b) (Legals cs) d =
  runParser (mkParam maker forms) ["--" ++ b ++ "=" ++ d] == Right d
  where
    forms = as ++ ["--" ++ b] ++ cs

prop_param_Finishes maker (Legals as) (Legal b) (Legals cs) d es =
  runParser (mkParam maker forms *> args) (b:d:es) == Right es
  where
    forms = as ++ [b] ++ cs

prop_param_FinishesShort
  maker (Legals as) (NotDash b) (Legals cs) (NonEmpty d) es =
  runParser (mkParam maker forms *> args) (('-':b:d):es) == Right es
  where
    forms = as ++ [['-', b]] ++ cs

prop_param_FinishesLong maker (Legals as) (NonEmpty b) (Legals cs) d es =
  runParser (mkParam maker forms *> args) (assign:es) == Right es
  where
    forms = as ++ ["--" ++ b] ++ cs
    assign = "--" ++ b ++ "=" ++ d

prop_param_Skips maker (Legals as) (Legal b) (Legals cs) d e =
  not (any (`isPrefixOf` e) forms) ==>
  runParser (mkParam maker forms #> args) [e, b, d] == Right [e]
  where
    forms = as ++ [b] ++ cs

prop_param_Empty maker (Legal a) (Legals bs) =
  isLeft $ runParser (mkParam maker forms) []
  where
    forms = a:bs

prop_param_MissingArg maker (Legals as) (Legal b) (Legals cs) =
  isLeft $ runParser (mkParam maker forms) [b]
  where
    forms = as ++ [b] ++ cs

prop_param_NotMatches maker (Legal a) (Legals bs) c d =
  not (any (`isPrefixOf` c) forms) ==>
  isLeft $ runParser (mkParam maker forms *> args) [c, d]
  where
    forms = a:bs


data FreeArgMaker
  = FreeArg' String
  | FreeArg String String
  deriving Show

mkFreeArg :: FreeArgMaker -> Parser String
mkFreeArg (FreeArg' meta) = freeArg' meta
mkFreeArg (FreeArg meta desc) = freeArg meta desc

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
  runParser (mkFreeArg maker) [a] == Right a

prop_freeArg_Finishes maker (Free a) bs =
  runParser (mkFreeArg maker *> args) (a:bs) == Right bs

prop_freeArg_Skips maker (Free a) b =
  runParser (mkFreeArg maker #> args) ['-':b, a] == Right ['-':b]

prop_freeArg_Empty maker =
  isLeft $ runParser (mkFreeArg maker) []

prop_freeArg_NotMatches maker a =
  isLeft $ runParser (mkFreeArg maker) ['-':a]


data MultiParamMaker
  = MultiParam'
  | MultiParam String
  deriving Show

mkMultiParam :: MultiParamMaker -> [OptionForm] -> Follower a -> Parser a
mkMultiParam MultiParam' opts f = multiParam' opts f
mkMultiParam (MultiParam desc) opts f = multiParam opts f desc

instance Arbitrary MultiParamMaker where
  arbitrary = oneof [pure MultiParam', MultiParam <$> arbitrary]

  shrink MultiParam' = []
  shrink (MultiParam desc) = MultiParam':
    [MultiParam desc' | desc' <- shrink desc]

prop_multiParam_Matches maker (Legals as) (Legal b) (Legals cs) dms =
  runParser (mkMultiParam maker forms $ traverse next ms) (b:ds) == Right ds
  where
    forms = as ++ [b] ++ cs
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam_Finishes maker (Legals as) (Legal b) (Legals cs) dms es =
  runParser (mkMultiParam maker forms f *> args) ([b] ++ ds ++ es) == Right es
  where
    forms = as ++ [b] ++ cs
    f = traverse next ms
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam_Skips maker (Legals as) (Legal b) (Legals cs) dms e =
  not (e `elem` forms) ==>
  runParser (mkMultiParam maker forms f #> args) (e:b:ds) == Right [e]
  where
    forms = as ++ [b] ++ cs
    f = traverse next ms
    ms = map snd dms  -- Metavariables.
    ds = map fst dms  -- Arguments that should match them.

prop_multiParam_Empty maker (Legal a) (Legals bs) ms =
  isLeft $ runParser (mkMultiParam maker forms f) []
  where
    forms = a:bs
    f :: Follower [String]
    f = traverse next ms

prop_multiParam_NotEnough
  maker (Legals as) (Legal b) (Legals cs) dms (NonEmpty ms) =
  isLeft $ runParser (mkMultiParam maker forms $ traverse next ms') (b:ds)
  where
    forms = as ++ [b] ++ cs
    ms' = map snd dms ++ ms  -- Metavariables.
    ds = map fst dms         -- Arguments that should match part of them.

prop_multiParam_NotMatches maker (Legal a) (Legals bs) ms c cs =
  not (c `elem` forms) ==>
  isLeft $ runParser (mkMultiParam maker forms f *> args) (c:cs)
  where
    forms = a:bs
    f :: Follower [String]
    f = traverse next ms


-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

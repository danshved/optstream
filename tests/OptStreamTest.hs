{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Either
import Data.List
import GHC.Generics
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Options.OptStream
import Options.OptStream.Test.Helpers hiding (null)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "flag"
    [ testProperty "Matches"       prop_flag_Matches
    , testProperty "Finishes"      prop_flag_Finishes
    , testProperty "Empty"         prop_flag_Empty
    , testProperty "NotMatches"    prop_flag_NotMatches
    , testProperty "Skips"         prop_flag_Skips
    , testProperty "MatchesBundle" prop_flag_MatchesBundle
    ]

  , testGroup "flagSep"
    [ testProperty "Matches"          prop_flagSep_Matches
    , testProperty "Finishes"         prop_flagSep_Finishes
    , testProperty "Empty"            prop_flagSep_Empty
    , testProperty "NotMatches"       prop_flagSep_NotMatches
    , testProperty "Skips"            prop_flagSep_Skips
    , testProperty "NotMatchesBundle" prop_flagSep_NotMatchesBundle
    ]

  , testGroup "param"
    [ testProperty "Matches"       prop_param_Matches
    , testProperty "Finishes"      prop_param_Finishes
    , testProperty "Empty"         prop_param_Empty
    , testProperty "MissingArg"    prop_param_MissingArg
    , testProperty "NotMatches"    prop_param_NotMatches
    , testProperty "Skips"         prop_param_Skips
    ]

  , testGroup "freeArg"
    [ testProperty "Matches"    prop_freeArg_Matches
    , testProperty "Finishes"   prop_freeArg_Finishes
    , testProperty "Empty"      prop_freeArg_Empty
    , testProperty "NotMatches" prop_freeArg_NotMatches
    , testProperty "Skips"      prop_freeArg_Skips
    ]

  , testGroup "multiParam"
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


-- TODO: hide Legal and Legals from the constructor below to make Show output
-- more readable.

-- | Represents a set of legal option forms with one of them selected.
data Forms = Forms Legals Legal Legals
  deriving (Show, Generic)

allForms :: Forms -> [OptionForm]
allForms (Forms (Legals as) (Legal b) (Legals cs)) = as ++ [b] ++ cs

chosenForm :: Forms -> OptionForm
chosenForm (Forms _ (Legal x) _) = x

instance Arbitrary Forms where
  arbitrary = Forms <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

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

-- | An example of a parse that should succeed.
data Example a = Example
  { parser :: Parser a
    -- ^ Parser under test.
  , inputs :: [String]
    -- ^ Example sequence of input arguments that the parser should
    -- successfully consume.
  , result :: a
    -- ^ The value that the parser should produce.
  , consumes :: ArgLanguage
    -- ^ The set of all strings the parser is supposed to be willing to
    -- consume. The parser should skip any string not belonging to this set.
  }

-- * Tests for 'flag' and 'flag''.

-- | Represents a choice between 'flag' and 'flag''.
data FlagMaker
  = Flag'
  | Flag String
  deriving (Show, Generic)

mkFlag :: FlagMaker -> [OptionForm] -> Parser ()
mkFlag Flag' opts = flag' opts
mkFlag (Flag desc) opts = flag opts desc

mkFlagSep :: FlagMaker -> [OptionForm] -> Parser ()
mkFlagSep Flag' opts = flagSep' opts
mkFlagSep (Flag desc) opts = flagSep opts desc


instance Arbitrary FlagMaker where
  arbitrary = oneof [pure Flag', Flag <$> arbitrary]

  shrink Flag' = []
  shrink m@(Flag _)= Flag':genericShrink m


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



-- * Tests for 'param' and co.

mkParam :: HelpChoice -> ValueType -> [OptionForm] -> String -> Parser String
mkParam WithoutHelp TypeString opts metavar = param' opts metavar
mkParam WithoutHelp TypeReadInt opts metavar =
  show <$> (paramRead' opts metavar :: Parser Int)
mkParam WithoutHelp TypeChar opts metavar = (:[]) <$> paramChar' opts metavar
mkParam (WithHelp desc) TypeString opts metavar = param opts metavar desc
mkParam (WithHelp desc) TypeReadInt opts metavar =
  show <$> (paramRead opts metavar desc :: Parser Int)
mkParam (WithHelp desc) TypeChar opts metavar =
  (:[]) <$> paramChar opts metavar desc

-- TODO: introduce helpers like Forms, but separate for short and long selected
-- form.

-- | Represents an example where a specific @param*@ parser should match a
-- specific block of arguments.
data ParamExampleBuilder
  = ParamExample HelpChoice Forms String Value
  | ParamShortExample HelpChoice Legals NotDash Legals String NonEmptyValue
  | ParamLongExample HelpChoice Legals (NonEmptyList Char) Legals String Value
  deriving (Show, Generic)

buildParamExample :: ParamExampleBuilder -> Example String
buildParamExample (ParamExample help fs metavar val)
  = Example
  { parser = mkParam help (valueType val) (allForms fs) metavar
  , inputs = [chosenForm fs, x]
  , result = x
  , consumes = mconcat . map withPrefix $ allForms fs
  }
  where
    x = formatValue val
buildParamExample
  (ParamShortExample help (Legals as) (NotDash b) (Legals cs) metavar (NonEmptyValue val))
  = Example
  { parser = mkParam help (valueType val) forms metavar
  , inputs = ['-':b:x]
  , result = x
  , consumes = mconcat $ map withPrefix forms
  }
  where
    forms = as ++ [['-', b]] ++ cs
    x = formatValue val
buildParamExample
  (ParamLongExample help (Legals as) (NonEmpty b) (Legals cs) metavar val)
  = Example
  { parser = mkParam help (valueType val) forms metavar
  , inputs = ["--" ++ b ++ "=" ++ x]
  , result = x
  , consumes = mconcat $ map withPrefix forms
  }
  where
    forms = as ++ ["--" ++ b] ++ cs
    x = formatValue val

instance Arbitrary ParamExampleBuilder where
  arbitrary = oneof
    [ ParamExample
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ParamShortExample
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
    , ParamLongExample
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        <*> arbitrary <*> arbitrary
    ]

  shrink = genericShrink


prop_param_Matches builder =
  runParser (parser ex) (inputs ex) == Right (result ex)
  where
    ex = buildParamExample builder

prop_param_Finishes builder y =
  runParser (parser ex *> args) (inputs ex ++ [y]) == Right [y]
  where
    ex = buildParamExample builder

prop_param_Skips builder y =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) == Right [y]
  where
    ex = buildParamExample builder

prop_param_Empty help valueType metavar fs =
  isLeft $ runParser parser []
  where
    parser = mkParam help valueType (allForms fs) metavar

prop_param_MissingArg help valueType metavar fs =
  isLeft $ runParser parser [chosenForm fs]
  where
    parser = mkParam help valueType (allForms fs) metavar

prop_param_NotMatches help valueType metavar fs c d =
  not (any (`isPrefixOf` c) forms) ==>
  isLeft $ runParser (parser *> args) [c, d]
  where
    forms = allForms fs
    parser = mkParam help valueType forms metavar


-- * Tests for 'freeArg' and co.

newtype NonEmptyValue = NonEmptyValue { unNonEmptyValue :: Value }
  deriving Show

instance Arbitrary NonEmptyValue where
  arbitrary = NonEmptyValue
    <$> (arbitrary `suchThat` (not . null . formatValue))

  shrink (NonEmptyValue x) =
    [ NonEmptyValue x'
    | x' <- shrink x
    , not . null $ formatValue x'
    ]

newtype FreeValue = FreeValue { unFreeValue :: Value }
  deriving Show

instance Arbitrary FreeValue where
  arbitrary = FreeValue <$> (arbitrary `suchThat` (isFree . formatValue))

  -- TODO: introduce a helper for such shrinks.
  shrink (FreeValue x) =
    [ FreeValue x'
    | x' <- shrink x
    , isFree $ formatValue x'
    ]

mkFreeArg :: HelpChoice -> ValueType -> String -> Parser String
mkFreeArg WithoutHelp TypeString mv = freeArg' mv
mkFreeArg WithoutHelp TypeReadInt mv = show <$> (freeArgRead' mv :: Parser Int)
mkFreeArg WithoutHelp TypeChar mv = (:[]) <$> freeArgChar' mv
mkFreeArg (WithHelp desc) TypeString mv = freeArg mv desc
mkFreeArg (WithHelp desc) TypeReadInt mv =
  show <$> (freeArgRead mv desc :: Parser Int)
mkFreeArg (WithHelp desc) TypeChar mv = (:[]) <$> freeArgChar mv desc


data FreeArgExampleBuilder
  = FreeArgExample HelpChoice String FreeValue
  deriving (Show, Generic)

buildFreeArgExample :: FreeArgExampleBuilder -> Example String
buildFreeArgExample (FreeArgExample help metavar (FreeValue val))
  = Example
  { parser = mkFreeArg help (valueType val) metavar
  , inputs = [x]
  , result = x
  , consumes = freeArgs
  }
  where
    x = formatValue val

instance Arbitrary FreeArgExampleBuilder where
  arbitrary = FreeArgExample <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink


prop_freeArg_Matches builder =
  runParser (parser ex) (inputs ex) == Right (result ex)
  where
    ex = buildFreeArgExample builder

prop_freeArg_Finishes builder y =
  runParser (parser ex *> args) (inputs ex ++ [y]) == Right [y]
  where
    ex = buildFreeArgExample builder

prop_freeArg_Skips builder y =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) == Right [y]
  where
    ex = buildFreeArgExample builder

prop_freeArg_Empty help valueType metavar =
  isLeft $ runParser (mkFreeArg help valueType metavar) []

prop_freeArg_NotMatches help valueType metavar a =
  isLeft $ runParser (mkFreeArg help valueType metavar) ['-':a]


-- | Represents a choice between a parser with help (e.g. 'param') or without
-- help (e.g. 'param'').
data HelpChoice
  = WithoutHelp
  | WithHelp String
  deriving (Show, Generic)

instance Arbitrary HelpChoice where
  arbitrary = oneof [pure WithoutHelp, WithHelp <$> arbitrary]

  shrink WithoutHelp = []
  shrink x@(WithHelp _) = WithoutHelp:genericShrink x


-- | Represents a choice between e.g. 'param', 'paramRead', and 'paramChar'.
data ValueType
  = TypeString
  | TypeReadInt
  | TypeChar
  deriving (Show, Generic)

instance Arbitrary ValueType where
  arbitrary = elements [TypeString, TypeReadInt, TypeChar]

  shrink TypeString = []
  shrink _ = [TypeString]


-- | Represents a test value to be parsed with 'param', 'freeArg', or 'next'.
data Value
  = ValueString String
  | ValueReadInt Int
  | ValueChar Char
  deriving (Show, Generic)

valueType :: Value -> ValueType
valueType (ValueString _) = TypeString
valueType (ValueReadInt _) = TypeReadInt
valueType (ValueChar _) = TypeChar

formatValue :: Value -> String
formatValue (ValueString s) = s
formatValue (ValueReadInt i) = show i
formatValue (ValueChar c) = [c]

instance Arbitrary Value where
  arbitrary = oneof
    [ ValueString <$> arbitrary
    , ValueReadInt <$> arbitrary
    , ValueChar <$> arbitrary
    ]

  shrink x@(ValueString _) = genericShrink x
  shrink x = ValueString (formatValue x):genericShrink x


mkMultiParam :: HelpChoice -> [OptionForm] -> Follower a -> Parser a
mkMultiParam WithoutHelp opts f = multiParam' opts f
mkMultiParam (WithHelp desc) opts f = multiParam opts f desc

mkNext :: ValueType -> String -> Follower String
mkNext TypeString metavar = next metavar
mkNext TypeReadInt metavar = show <$> (nextRead metavar :: Follower Int)
mkNext TypeChar metavar = (:[]) <$> nextChar metavar


data MultiParamExampleBuilder
  = MultiParamExample HelpChoice Forms [(String, Value)]
  deriving (Show, Generic)

buildMultiParamExample :: MultiParamExampleBuilder -> Example [String]
buildMultiParamExample (MultiParamExample helpChoice fs pairs) = Example
  { parser = mkMultiParam helpChoice (allForms fs) $ traverse toNext pairs
  , inputs = chosenForm fs:xs
  , result = xs
  , consumes = mconcat . map singleton $ allForms fs
  }
  where
    toNext (metavar, val) = mkNext (valueType val) metavar
    xs = map (formatValue . snd) pairs

instance Arbitrary MultiParamExampleBuilder where
  arbitrary =
    MultiParamExample <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink


prop_multiParam_Matches builder =
  runParser (parser ex) (inputs ex) == Right (result ex)
  where
    ex = buildMultiParamExample builder

prop_multiParam_Finishes builder y =
  runParser (parser ex *> args) (inputs ex ++ [y]) == Right [y]
  where
    ex = buildMultiParamExample builder

prop_multiParam_Skips builder y =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) == Right [y]
  where
    ex = buildMultiParamExample builder

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

-- TODO: test *Read and *Char as well.
-- TODO: Use Forms instead of Legals where possible.
-- TODO: improve distribution of arbitrary argument strings.
-- TODO: test that defaults for all atomic parsers can be added with <|> orElse.

-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

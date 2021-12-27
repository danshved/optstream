{-# LANGUAGE DeriveGeneric #-}  -- TODO: remove from here.
module Main where

import Data.Either
import Data.List
import GHC.Generics  -- TODO: remove from here.
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

-- Helper parser that collects all the arguments that it gets.
args :: Parser [String]
args = many (anyArg' "ARG")



-- * Tests for @flag*@

-- TOODO: get rid, move mk* to Helpers.
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



-- * Tests for @param*@

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



-- * Tests for @freeArg*@

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



-- * Tests for @multiParam*@

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


-- TODO: Use Forms instead of Legals where possible.
-- TODO: improve distribution of arbitrary argument strings.
-- TODO: test that defaults for all atomic parsers can be added with <|> orElse.

-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

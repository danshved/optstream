module Main where

import Data.Either
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Options.OptStream
import Options.OptStream.Test.Helpers hiding (null)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "flag*"
    [ testProperty "Matches"          prop_flag_Matches
    , testProperty "Finishes"         prop_flag_Finishes
    , testProperty "Skips"            prop_flag_Skips
    , testProperty "Empty"            prop_flag_Empty
    , testProperty "NotMatches"       prop_flag_NotMatches
    , testProperty "MatchesBundle"    prop_flag_MatchesBundle
    , testProperty "NotMatchesBundle" prop_flag_NotMatchesBundle
    ]

  , testGroup "param*"
    [ testProperty "Matches"       prop_param_Matches
    , testProperty "Finishes"      prop_param_Finishes
    , testProperty "Skips"         prop_param_Skips
    , testProperty "Empty"         prop_param_Empty
    , testProperty "NotMatches"    prop_param_NotMatches
    , testProperty "MissingArg"    prop_param_MissingArg
    ]

  , testGroup "freeArg*"
    [ testProperty "Matches"    prop_freeArg_Matches
    , testProperty "Finishes"   prop_freeArg_Finishes
    , testProperty "Skips"      prop_freeArg_Skips
    , testProperty "Empty"      prop_freeArg_Empty
    , testProperty "NotMatches" prop_freeArg_NotMatches
    ]

  , testGroup "multiParam*"
    [ testProperty "Matches"    prop_multiParam_Matches
    , testProperty "Finishes"   prop_multiParam_Finishes
    , testProperty "Skips"      prop_multiParam_Skips
    , testProperty "Empty"      prop_multiParam_Empty
    , testProperty "NotMatches" prop_multiParam_NotMatches
    , testProperty "NotEnough"  prop_multiParam_NotEnough
    ]
  ]

-- | Helper parser that collects all the arguments that it gets.
args :: Parser [String]
args = many (anyArg' "ARG")



-- * Tests for @flag*@

prop_flag_Matches help bundling fs =
  runParser parser [chosenForm fs] == Right ()
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_Finishes help bundling fs x =
  runParser (parser *> args) [chosenForm fs, x] == Right [x]
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_Skips help bundling fs x =
  not (x `elem` forms) ==>
  runParser (parser #> args) [x, chosenForm fs] == Right [x]
  where
    forms = allForms fs
    parser = mkFlag help bundling forms

prop_flag_Empty help bundling fs =
  isLeft $ runParser parser []
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_NotMatches help bundling fs x =
  not (x `elem` forms) ==>
  isLeft $ runParser parser [x]
  where
    forms = allForms fs
    parser = mkFlag help bundling forms

prop_flag_MatchesBundle help (NonEmpty cs) =
  runParser parser ['-':chars] == Right [() | c <- chars]
  where
    chars = [c | NotDash c <- cs]
    parser = sequenceA [mkFlag help WithBundling [['-', c]] | c <- chars]

prop_flag_NotMatchesBundle help cs =
  length chars >= 2 ==>
  isLeft $ runParser parser ['-':chars]
  where
    chars = [c | NotDash c <- cs]
    parser = sequenceA [mkFlag help WithoutBundling [['-', c]] | c <- chars]



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

prop_param_NotMatches help valueType metavar fs c d =
  not (any (`isPrefixOf` c) forms) ==>
  isLeft $ runParser (parser *> args) [c, d]
  where
    forms = allForms fs
    parser = mkParam help valueType forms metavar

prop_param_MissingArg help valueType metavar fs =
  isLeft $ runParser parser [chosenForm fs]
  where
    parser = mkParam help valueType (allForms fs) metavar



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

prop_multiParam_NotMatches maker (Legal a) (Legals bs) ms c cs =
  not (c `elem` forms) ==>
  isLeft $ runParser (mkMultiParam maker forms f *> args) (c:cs)
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


-- TODO: Use Forms instead of Legals where possible.
-- TODO: improve distribution of arbitrary argument strings.
-- TODO: test that defaults for all atomic parsers can be added with <|> orElse.

-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

module Main where

import Control.Applicative hiding (some, many, optional)
import Data.Functor
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Options.OptStream
import Options.OptStream.Help
import Options.OptStream.Test.Helpers hiding (null)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "flag"
    [ testProperty "Matches"          prop_flag_Matches
    , testProperty "Finishes"         prop_flag_Finishes
    , testProperty "Skips"            prop_flag_Skips
    , testProperty "Empty"            prop_flag_Empty
    , testProperty "OrElse"           prop_flag_OrElse
    , testProperty "NotMatches"       prop_flag_NotMatches
    , testProperty "MatchesBundle"    prop_flag_MatchesBundle
    , testProperty "NotMatchesBundle" prop_flag_NotMatchesBundle
    , testProperty "Help"             prop_flag_Help
    , testProperty "NoHelp"           prop_flag_NoHelp
    , testProperty "EmptyForms"       prop_flag_EmptyForms
    , testProperty "IllegalForm"      prop_flag_IllegalForm
    ]

  , testGroup "param"
    [ testProperty "Matches"       prop_param_Matches
    , testProperty "Finishes"      prop_param_Finishes
    , testProperty "Skips"         prop_param_Skips
    , testProperty "Empty"         prop_param_Empty
    , testProperty "OrElse"        prop_param_OrElse
    , testProperty "NotMatches"    prop_param_NotMatches
    , testProperty "MissingArg"    prop_param_MissingArg
    , testProperty "Help"          prop_param_Help
    , testProperty "NoHelp"        prop_param_NoHelp
    , testProperty "EmptyForms"    prop_param_EmptyForms
    , testProperty "IllegalForm"   prop_param_IllegalForm
    ]

  , testGroup "freeArg"
    [ testProperty "Matches"    prop_freeArg_Matches
    , testProperty "Finishes"   prop_freeArg_Finishes
    , testProperty "Skips"      prop_freeArg_Skips
    , testProperty "Empty"      prop_freeArg_Empty
    , testProperty "OrElse"     prop_freeArg_OrElse
    , testProperty "NotMatches" prop_freeArg_NotMatches
    , testProperty "Help"       prop_freeArg_Help
    , testProperty "NoHelp"     prop_freeArg_NoHelp
    ]

  , testGroup "multiParam"
    [ testProperty "Matches"     prop_multiParam_Matches
    , testProperty "Finishes"    prop_multiParam_Finishes
    , testProperty "Skips"       prop_multiParam_Skips
    , testProperty "Empty"       prop_multiParam_Empty
    , testProperty "OrElse"      prop_multiParam_OrElse
    , testProperty "NotMatches"  prop_multiParam_NotMatches
    , testProperty "NotEnough"   prop_multiParam_NotEnough
    , testProperty "Help"        prop_multiParam_Help
    , testProperty "NoHelp"      prop_multiParam_NoHelp
    , testProperty "EmptyForms"  prop_multiParam_EmptyForms
    , testProperty "IllegalForm" prop_multiParam_IllegalForm
    ]
  ]

-- | Helper parser that collects all the arguments that it gets.
args :: Parser [String]
args = many (anyArg' "ARG")



-- * Tests for @flag*@

prop_flag_Matches help bundling fs =
  runParser parser [chosenForm fs] === Right ()
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_Finishes help bundling fs (AnyArg x) =
  runParser (parser *> args) [chosenForm fs, x] === Right [x]
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_Skips help bundling fs (AnyArg x) =
  not (x `elem` forms) ==>
  runParser (parser #> args) [x, chosenForm fs] === Right [x]
  where
    forms = allForms fs
    parser = mkFlag help bundling forms

prop_flag_Empty help bundling fs =
  isLeft' $ runParser parser []
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_OrElse help bundling fs =
  runParser (parser $> True <|> orElse False) [] === Right False
  where
    parser = mkFlag help bundling (allForms fs)

prop_flag_NotMatches help bundling fs (AnyArg x) =
  not (x `elem` forms) ==>
  isLeft' $ runParser parser [x]
  where
    forms = allForms fs
    parser = mkFlag help bundling forms

prop_flag_MatchesBundle help (NonEmpty cs) =
  runParser parser ['-':chars] === Right [() | c <- chars]
  where
    chars = [c | NotDash c <- cs]
    parser = sequenceA [mkFlag help WithBundling [['-', c]] | c <- chars]

prop_flag_NotMatchesBundle help cs =
  length chars >= 2 ==>
  isLeft' $ runParser parser ['-':chars]
  where
    chars = [c | NotDash c <- cs]
    parser = sequenceA [mkFlag help WithoutBundling [['-', c]] | c <- chars]

prop_flag_Help desc bundling fs =
  getHelp (mkFlag (WithHelp desc) bundling (allForms fs)) =/= mempty

prop_flag_NoHelp bundling fs =
  getHelp (mkFlag WithoutHelp bundling (allForms fs)) === mempty

prop_flag_EmptyForms help bundling =
  throwsError . toRaw $ mkFlag help bundling []

prop_flag_IllegalForm help bundling (ChosenIllegal fs) =
  throwsError . toRaw $ mkFlag help bundling (allForms fs)



-- * Tests for @param*@

prop_param_Matches builder =
  runParser (parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildParamExample builder

prop_param_Finishes builder (AnyArg y) =
  runParser (parser ex *> args) (inputs ex ++ [y]) === Right [y]
  where
    ex = buildParamExample builder

prop_param_Skips builder (AnyArg y) =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) === Right [y]
  where
    ex = buildParamExample builder

prop_param_Empty help valueType metavar fs =
  isLeft' $ runParser parser []
  where
    parser = mkParam help valueType (allForms fs) metavar

prop_param_OrElse help valueType metavar fs x =
  runParser (parser <|> orElse x) [] === Right x
  where
    parser = mkParam help valueType (allForms fs) metavar

prop_param_NotMatches help valueType metavar fs (AnyArg c) (AnyArg d) =
  not (any (`isPrefixOf` c) forms) ==>
  isLeft' $ runParser (parser *> args) [c, d]
  where
    forms = allForms fs
    parser = mkParam help valueType forms metavar

prop_param_MissingArg help valueType metavar fs =
  isLeft' $ runParser parser [chosenForm fs]
  where
    parser = mkParam help valueType (allForms fs) metavar

prop_param_Help desc valueType metavar fs =
  getHelp (mkParam (WithHelp desc) valueType (allForms fs) metavar) =/= mempty

prop_param_NoHelp valueType metavar fs =
  getHelp (mkParam WithoutHelp valueType (allForms fs) metavar) === mempty

prop_param_EmptyForms help valueType metavar =
  throwsError . toRaw $  mkParam help valueType [] metavar

prop_param_IllegalForm help valueType (ChosenIllegal fs) metavar =
  throwsError . toRaw $  mkParam help valueType (allForms fs) metavar



-- * Tests for @freeArg*@

prop_freeArg_Matches builder =
  runParser (parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildFreeArgExample builder

prop_freeArg_Finishes builder (AnyArg y) =
  runParser (parser ex *> args) (inputs ex ++ [y]) === Right [y]
  where
    ex = buildFreeArgExample builder

prop_freeArg_Skips builder (AnyArg y) =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) === Right [y]
  where
    ex = buildFreeArgExample builder

prop_freeArg_Empty help valueType metavar =
  isLeft' $ runParser (mkFreeArg help valueType metavar) []

prop_freeArg_OrElse help valueType metavar x =
  runParser (parser <|> orElse x) [] === Right x
  where
    parser = mkFreeArg help valueType metavar

prop_freeArg_NotMatches help valueType metavar a =
  isLeft' $ runParser (mkFreeArg help valueType metavar) ['-':a]

prop_freeArg_Help desc valueType metavar =
  getHelp (mkFreeArg (WithHelp desc) valueType metavar) =/= mempty

prop_freeArg_NoHelp valueType metavar =
  getHelp (mkFreeArg WithoutHelp valueType metavar) === mempty



-- * Tests for @multiParam*@

prop_multiParam_Matches builder =
  runParser (parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildMultiParamExample builder

prop_multiParam_Finishes builder (AnyArg y) =
  runParser (parser ex *> args) (inputs ex ++ [y]) === Right [y]
  where
    ex = buildMultiParamExample builder

prop_multiParam_Skips builder (AnyArg y) =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) === Right [y]
  where
    ex = buildMultiParamExample builder

prop_multiParam_Empty help fs pairs =
  isLeft' $ runParser parser []
  where
    parser = mkMultiParam help (allForms fs) (mkFollower pairs)

prop_multiParam_OrElse help fs pairs x =
  runParser (parser <|> orElse x) [] === Right x
  where
    parser = mkMultiParam help (allForms fs) (mkFollower pairs)

prop_multiParam_NotMatches help fs pairs (AnyArg c) (AnyArgs cs) =
  not (c `elem` forms) ==>
  isLeft' $ runParser (parser *> args) (c:cs)
  where
    forms = allForms fs
    parser = mkMultiParam help (allForms fs) (mkFollower pairs)

prop_multiParam_NotEnough help fs matches (NonEmpty pairs) =
  isLeft' $ runParser parser (chosenForm fs:ds)
  where
    ds = [formatValue val | (_, val) <- matches]
    pairs' = [(valueType val, mv) | (mv, val) <- matches] ++ pairs
    parser = mkMultiParam help (allForms fs) (mkFollower pairs')

prop_multiParam_Help desc fs pairs =
  getHelp parser =/= mempty
  where
    parser = mkMultiParam (WithHelp desc) (allForms fs) (mkFollower pairs)

prop_multiParam_NoHelp fs pairs =
  getHelp parser === mempty
  where
    parser = mkMultiParam WithoutHelp (allForms fs) (mkFollower pairs)

prop_multiParam_EmptyForms help pairs =
  throwsError . toRaw $ mkMultiParam help [] (mkFollower pairs)

prop_multiParam_IllegalForm help (ChosenIllegal fs) pairs =
  throwsError . toRaw $ mkMultiParam help (allForms fs) (mkFollower pairs)



-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

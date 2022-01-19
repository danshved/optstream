{-|
Module      : Main
Copyright   : (c) Dan Shved, 2022
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

QuickCheck tests for "Options.OptStream".
-}

{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative hiding (some, many, optional)
import Control.Monad
import Data.Either
import Data.Functor
import Data.List
import Prelude hiding (putStrLn)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Options.OptStream
import Options.OptStream.Help
import Options.OptStream.IOOps
import Options.OptStream.Test.Helpers hiding (null, empty)
import Options.OptStream.Test.TestIO hiding (args)

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

  , testGroup "match"
    [ testProperty "Matches"    prop_match_Matches
    , testProperty "Finishes"   prop_match_Finishes
    , testProperty "Skips"      prop_match_Skips
    , testProperty "Empty"      prop_match_Empty
    , testProperty "OrElse"     prop_match_OrElse
    , testProperty "NotMatches" prop_match_NotMatches
    , testProperty "NoHelp"     prop_match_NoHelp
    ]

  , testGroup "matchAndFollow"
    [ testProperty "Matches"    prop_matchAndFollow_Matches
    , testProperty "Finishes"   prop_matchAndFollow_Finishes
    , testProperty "Skips"      prop_matchAndFollow_Skips
    , testProperty "Empty"      prop_matchAndFollow_Empty
    , testProperty "OrElse"     prop_matchAndFollow_OrElse
    , testProperty "NotMatches" prop_matchAndFollow_NotMatches
    , testProperty "NotEnough"  prop_matchAndFollow_NotEnough
    , testProperty "NoHelp"     prop_matchAndFollow_NoHelp
    ]

  , testGroup "matchShort"
    [ testProperty "Matches"          prop_matchShort_Matches
    , testProperty "Finishes"         prop_matchShort_Finishes
    , testProperty "Skips"            prop_matchShort_Skips
    , testProperty "FinishesInBundle" prop_matchShort_FinishesInBundle
    , testProperty "SkipsInBundle"    prop_matchShort_SkipsInBundle
    , testProperty "Empty"            prop_matchShort_Empty
    , testProperty "OrElse"           prop_matchShort_OrElse
    , testProperty "NotMatches"       prop_matchShort_NotMatches
    , testProperty "MatchesBundle"    prop_matchShort_MatchesBundle
    , testProperty "NoHelp"           prop_matchShort_NoHelp
    ]

  , testGroup "withHelp"
    [ testProperty "MatchesMain"       prop_withHelp_MatchesMain
    , testProperty "MatchesHelp"       prop_withHelp_MatchesHelp
    , testProperty "MatchesHelpMiddle" prop_withHelp_MatchesHelpMiddle
    , testProperty "AddsHelp"          prop_withHelp_AddsHelp
    ]

  , testGroup "withHelp'"
    [ testProperty "MatchesMain"       prop_withHelp'_MatchesMain
    , testProperty "MatchesHelp"       prop_withHelp'_MatchesHelp
    , testProperty "MatchesHelpMiddle" prop_withHelp'_MatchesHelpMiddle
    , testProperty "AddsNoHelp"        prop_withHelp'_AddsNoHelp
    ]

  , testGroup "withSubHelp"
    [ testProperty "MatchesMain"       prop_withSubHelp_MatchesMain
    , testProperty "MatchesHelp"       prop_withSubHelp_MatchesHelp
    , testProperty "MatchesHelpMiddle" prop_withSubHelp_MatchesHelpMiddle
    , testProperty "ClearsHelp"        prop_withSubHelp_ClearsHelp
    ]

  , testGroup "withSubHelp'"
    [ testProperty "MatchesMain"       prop_withSubHelp'_MatchesMain
    , testProperty "MatchesHelp"       prop_withSubHelp'_MatchesHelp
    , testProperty "MatchesHelpMiddle" prop_withSubHelp'_MatchesHelpMiddle
    , testProperty "ClearsrHelp"       prop_withSubHelp'_ClearsHelp
    ]

  , testGroup "withVersion"
    [ testProperty "MatchesMain" prop_withVersion_MatchesMain
    , testProperty "MatchesHelp" prop_withVersion_MatchesVersion
    , testProperty "AddsHelp"    prop_withVersion_AddsHelp
    ]

  , testGroup "withVersion'"
    [ testProperty "MatchesMain" prop_withVersion'_MatchesMain
    , testProperty "MatchesHelp" prop_withVersion'_MatchesVersion
    , testProperty "AddsNoHelp"  prop_withVersion'_AddsNoHelp
    ]

  , testGroup "beforeDashes"
    [ testProperty "MatchesMain" prop_beforeDashes_MatchesMain
    , testProperty "Matches"     prop_beforeDashes_Matches
    , testProperty "Finishes"    prop_beforeDashes_Finishes
    , testProperty "AddsNoHelp"  prop_beforeDashes_AddsNoHelp
    ]

  , testGroup "quiet"
    [ testProperty "Matches"    prop_quiet_Matches
    , testProperty "AddsNoHelp" prop_quiet_AddsNoHelp
    ]

  , testGroup "eject"
    [ testProperty "Matches"         prop_eject_Matches
    , testProperty "Ejects"          prop_eject_Ejects
    , testProperty "EjectsAfter"     prop_eject_EjectsAfter
    , testProperty "EjectsMiddle"    prop_eject_EjectsMiddle
    , testProperty "EjectsLongAfter" prop_eject_EjectsLongAfter
    , testProperty "JoinsHelp"       prop_eject_JoinsHelp
    ]

  , testGroup "header"
    [ testProperty "Matches"        prop_header_Matches
    , testProperty "PrependsHeader" prop_header_PrependsHeader
    ]

  , testGroup "footer"
    [ testProperty "Matches"        prop_footer_Matches
    , testProperty "PrependsHeader" prop_footer_PrependsFooter
    ]

  , testGroup "flagHelp"
    [ testProperty "Matches"      prop_flagHelp_Matches
    , testProperty "PrependsHelp" prop_flagHelp_PrependsHelp
    , testProperty "IllegalForm"  prop_flagHelp_IllegalForm
    ]

  , testGroup "paramHelp"
    [ testProperty "Matches"      prop_paramHelp_Matches
    , testProperty "PrependsHelp" prop_paramHelp_PrependsHelp
    , testProperty "IllegalForm"  prop_paramHelp_IllegalForm
    ]

  , testGroup "freeArgHelp"
    [ testProperty "Matches"      prop_freeArgHelp_Matches
    , testProperty "PrependsHelp" prop_freeArgHelp_PrependsHelp
    ]

  , testGroup "multiParamHelp"
    [ testProperty "Matches"      prop_multiParamHelp_Matches
    , testProperty "PrependsHelp" prop_multiParamHelp_PrependsHelp
    , testProperty "IllegalForm"  prop_multiParamHelp_IllegalForm
    ]

  , testGroup "clearHelp"
    [ testProperty "Matches" prop_clearHelp_Matches
    , testProperty "ClearsHelp" prop_clearHelp_ClearsHelp
    ]

  , testGroup "clearHeader"
    [ testProperty "Matches" prop_clearHeader_Matches
    , testProperty "ClearsHeader" prop_clearHeader_ClearsHeader
    ]

  , testGroup "clearFooter"
    [ testProperty "Matches" prop_clearFooter_Matches
    , testProperty "ClearsFooter" prop_clearFooter_ClearsFooter
    ]

  , testGroup "clearTable"
    [ testProperty "Matches" prop_clearTable_Matches
    , testProperty "ClearsTable" prop_clearTable_ClearsTable
    ]

  , testGroup "sortTable"
    [ testProperty "Matches" prop_sortTable_Matches
    , testProperty "SortsTable" prop_sortTable_SortsTable
    ]

  , testGroup "runParserIO"
    [ testProperty "Returns" prop_runParserIO_Returns
    , testProperty "Dies"    prop_runParserIO_Dies
    ]

  , testGroup "parseArgs"
    [ testProperty "Returns" prop_parseArgs_Returns
    , testProperty "Dies"    prop_parseArgs_Dies
    ]

  , testGroup "parseArgsWithHelp"
    [ testProperty "Returns"    prop_parseArgsWithHelp_Returns
    , testProperty "PrintsHelp" prop_parseArgsWithHelp_PrintsHelp
    , testProperty "Dies"       prop_parseArgsWithHelp_Dies
    ]

  , testGroup "withHelpIO"
    [ testProperty "MatchesMain" prop_withHelpIO_MatchesMain
    , testProperty "MatchesHelp" prop_withHelpIO_MatchesHelp
    , testProperty "AddsHelp"    prop_withHelpIO_AddsHelp
    ]

  , testGroup "withHelpIO'"
    [ testProperty "MatchesMain" prop_withHelpIO'_MatchesMain
    , testProperty "MatchesHelp" prop_withHelpIO'_MatchesHelp
    , testProperty "AddsNoHelp"  prop_withHelpIO'_AddsNoHelp
    ]

  , testGroup "withSubHelpIO"
    [ testProperty "MatchesMain" prop_withSubHelpIO_MatchesMain
    , testProperty "MatchesHelp" prop_withSubHelpIO_MatchesHelp
    , testProperty "ClearsHelp"  prop_withSubHelpIO_ClearsHelp
    ]

  , testGroup "withSubHelpIO'"
    [ testProperty "MatchesMain" prop_withSubHelpIO'_MatchesMain
    , testProperty "MatchesHelp" prop_withSubHelpIO'_MatchesHelp
    , testProperty "ClearsHelp"  prop_withSubHelpIO'_ClearsHelp
    ]

  , testGroup "withVersionIO"
    [ testProperty "MatchesMain"    prop_withVersionIO_MatchesMain
    , testProperty "MatchesVersion" prop_withVersionIO_MatchesVersion
    , testProperty "AddsHelp"       prop_withVersionIO_AddsHelp
    ]

  , testGroup "withVersionIO'"
    [ testProperty "MatchesMain"    prop_withVersionIO'_MatchesMain
    , testProperty "MatchesVersion" prop_withVersionIO'_MatchesVersion
    , testProperty "AddsNoHelp"     prop_withVersionIO'_AddsNoHelp
    ]

  , testGroup "fmap"
    [ testProperty "MapsSuccess"   prop_fmap_MapsSuccess
    , testProperty "MapsAnyResult" prop_fmap_MapsAnyResult
    , testProperty "AddsNoHelp"    prop_fmap_AddsNoHelp
    ]

  , testGroup "fmapOrFail"
    [ testProperty "MapsSuccess"          prop_fmapOrFail_MapsSuccess
    , testProperty "MapsSuccessToFailure" prop_fmapOrFail_MapsSuccessToFailure
    , testProperty "MapsAnyToFailure"     prop_fmapOrFail_MapsAnyToFailure
    , testProperty "AddsNoHelp"           prop_fmapOrFail_AddsNoHelp
    ]

  , testGroup "pure"
    [ testProperty "Matches"    prop_pure_Matches
    , testProperty "Finishes"   prop_pure_Finishes
    , testProperty "NotMatches" prop_pure_NotMatches
    , testProperty "NoHelp"     prop_pure_NoHelp
    ]

  , testGroup "ap"
    [ testProperty "Matches"    prop_ap_Matches
    , testProperty "MatchesFar" prop_ap_MatchesFar
    , testProperty "JoinsHelp"  prop_ap_JoinsHelp
    ]

  , testGroup "failA"
    [ testProperty "Fails"  prop_failA_Fails
    , testProperty "OrElse" prop_failA_OrElse
    , testProperty "NoHelp" prop_failA_NoHelp
    ]

  , testGroup "empty"
    [ testProperty "Fails"  prop_empty_Fails
    , testProperty "OrElse" prop_empty_OrElse
    , testProperty "NoHelp" prop_empty_NoHelp
    ]

  , testGroup "alternative"
    [ testProperty "MatchesFirst"  prop_alternative_MatchesFirst
    , testProperty "MatchesSecond" prop_alternative_MatchesSecond
    , testProperty "LeftEmpty"     prop_alternative_LeftEmpty
    , testProperty "RightEmpty"    prop_alternative_RightEmpty
    , testProperty "JoinsHelp"     prop_alternative_JoinsHelp
    ]

  , testGroup "leftAlternative"
    [ testProperty "MatchesFirst"  prop_leftAlternative_MatchesFirst
    , testProperty "MatchesSecond" prop_leftAlternative_MatchesSecond
    , testProperty "MatchesMiddle" prop_leftAlternative_MatchesMiddle
    , testProperty "LeftEmpty"     prop_leftAlternative_LeftEmpty
    , testProperty "RightEmpty"    prop_leftAlternative_RightEmpty
    , testProperty "JoinsHelp"     prop_leftAlternative_JoinsHelp
    ]

  , testGroup "rightAlternative"
    [ testProperty "MatchesFirst"  prop_rightAlternative_MatchesFirst
    , testProperty "MatchesSecond" prop_rightAlternative_MatchesSecond
    , testProperty "MatchesMiddle" prop_rightAlternative_MatchesMiddle
    , testProperty "LeftEmpty"     prop_rightAlternative_LeftEmpty
    , testProperty "RightEmpty"    prop_rightAlternative_RightEmpty
    , testProperty "JoinsHelp"     prop_rightAlternative_JoinsHelp
    ]

  , testGroup "eof"
    [ testProperty "Empty"  prop_eof_Empty
    , testProperty "Skips"  prop_eof_Skips
    , testProperty "OrElse" prop_eof_OrElse
    , testProperty "NoHelp" prop_eof_NoHelp
    ]

  , testGroup "parallel"
    [ testProperty "DirectOrder"     prop_parallel_DirectOrder
    , testProperty "ReverseOrder"    prop_parallel_ReverseOrder
    , testProperty "DirectOrderFar"  prop_parallel_DirectOrderFar
    , testProperty "ReverseOrderFar" prop_parallel_ReverseOrderFar
    , testProperty "Mix"             prop_parallel_Mix
    ]

  , testGroup "leftParallel"
    [ testProperty "Matches"              prop_leftParallel_Matches
    , testProperty "NotMatchesReverse"    prop_leftParallel_NotMatchesReverse
    , testProperty "MatchesInterrupting"  prop_leftParallel_MatchesInterrupting
    , testProperty "FinishesInterrupting" prop_leftParallel_FinishesInterrupting
    , testProperty "JoinsHelp"            prop_leftParallel_JoinsHelp
    ]

  , testGroup "rightParallel"
    [ testProperty "Matches"              prop_rightParallel_Matches
    , testProperty "NotMatchesDirect"     prop_rightParallel_NotMatchesDirect
    , testProperty "MatchesInterrupting"  prop_rightParallel_MatchesInterrupting
    , testProperty "FinishesInterrupting" prop_rightParallel_FinishesInterrupting
    , testProperty "JoinsHelp"            prop_rightParallel_JoinsHelp
    ]

  , testGroup "many"
    [ testProperty "MatchesOne"  prop_many_MatchesOne
    , testProperty "MatchesZero" prop_many_MatchesZero
    , testProperty "AddsNoHelp"  prop_many_AddsNoHelp
    ]

  , testGroup "some"
    [ testProperty "MatchesOne"     prop_some_MatchesOne
    , testProperty "NotMatchesZero" prop_some_NotMatchesZero
    , testProperty "AddsNoHelp"     prop_some_AddsNoHelp
    ]

  , testGroup "optional"
    [ testProperty "Matches"      prop_optional_Matches
    , testProperty "MatchesEmpty" prop_optional_MatchesEmpty
    , testProperty "AddsNoHelp"   prop_optional_AddsNoHelp
    ]

  , testGroup "between"
    [ testProperty "Matches"     prop_between_Matches
    , testProperty "TooMany"     prop_between_TooMany
    , testProperty "TooFew"      prop_between_TooFew
    , testProperty "NegativeLow" prop_between_NegativeLow
    , testProperty "SmallHigh"   prop_between_SmallHigh
    , testProperty "AddsNoHelp"  prop_between_AddsNoHelp
    ]

  , testGroup "perm"
    [ testProperty "Matches"   prop_perm_Matches
    , testProperty "JoinsHelp" prop_perm_JoinsHelp
    ]
  ]


-- | Helper parser that collects all the arguments that it gets.
args :: Parser [String]
args = many (anyArg' "ARG")

-- | Helper parser that collects all the short flags that it gets.
shorts :: Parser [Char]
shorts = many (short "" Just)



-- * Tests for @flag*@

prop_flag_Matches builder =
  runParser (parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildFlagExample builder

prop_flag_Finishes builder (AnyArg y) =
  runParser (parser ex *> args) (inputs ex ++ [y]) === Right [y]
  where
    ex = buildFlagExample builder

prop_flag_Skips builder (AnyArg y) =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) === Right [y]
  where
    ex = buildFlagExample builder

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
  getHelp (mkFlag (WithHelp desc) bundling (allForms fs))
  === makeFlagHelp (allForms fs) desc

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
  getHelp (mkParam (WithHelp desc) valueType (allForms fs) metavar)
  === makeParamHelp (allForms fs) metavar desc

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
  getHelp (mkFreeArg (WithHelp desc) valueType metavar)
  === makeFreeArgHelp metavar desc

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
  getHelp (mkMultiParam (WithHelp desc) (allForms fs) follower)
  === makeMultiParamHelp (allForms fs) (getFollowerHelp follower) desc
  where
    follower = mkFollower pairs

prop_multiParam_NoHelp fs pairs =
  getHelp parser === mempty
  where
    parser = mkMultiParam WithoutHelp (allForms fs) (mkFollower pairs)

prop_multiParam_EmptyForms help pairs =
  throwsError . toRaw $ mkMultiParam help [] (mkFollower pairs)

prop_multiParam_IllegalForm help (ChosenIllegal fs) pairs =
  throwsError . toRaw $ mkMultiParam help (allForms fs) (mkFollower pairs)



-- * Tests for match

prop_match_Matches (AnyArg s) =
  runParser (match s) [s] === Right s

prop_match_Finishes (AnyArg s) (AnyArg y) =
  runParser (match s *> args) [s, y] === Right [y]

prop_match_Skips (AnyArg s) (AnyArg y) =
  runParser (match s #> args) [y, s] === Right [y]

prop_match_Empty (AnyArg s) =
  isLeft' $ runParser (match s) []

prop_match_OrElse (AnyArg s) x =
  runParser (match s <|> orElse x) [] === Right x

prop_match_NotMatches (AnyArg s) (AnyArg x) =
  x /= s ==>
  isLeft' $ runParser (match s) [x]

prop_match_NoHelp (AnyArg s) =
  getHelp (match s) === mempty



-- * Tests for matchAndFollow


prop_matchAndFollow_Matches builder =
  runParser (parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildMAFExample builder

prop_matchAndFollow_Finishes builder (AnyArg y) =
  runParser (parser ex *> args) (inputs ex ++ [y]) === Right [y]
  where
    ex = buildMAFExample builder

prop_matchAndFollow_Skips builder (AnyArg y) =
  not (y `member` consumes ex) ==>
  runParser (parser ex #> args) (y:inputs ex) === Right [y]
  where
    ex = buildMAFExample builder

prop_matchAndFollow_Empty (AnyArg s) pairs =
  isLeft' $ runParser (matchAndFollow s $ mkFollower pairs) []

prop_matchAndFollow_OrElse (AnyArg s) pairs x =
  runParser (parser <|> orElse x) [] === Right x
  where
    parser = matchAndFollow s $ mkFollower pairs

prop_matchAndFollow_NotMatches (AnyArg s) pairs (AnyArg s') (AnyArgs xs) =
  s' /= s ==>
  isLeft' $ runParser (parser *> args) (s':xs)
  where
    parser = matchAndFollow s $ mkFollower pairs

prop_matchAndFollow_NotEnough (AnyArg s) matches (NonEmpty pairs) =
  isLeft' $ runParser (matchAndFollow s $ mkFollower pairs') (s:ds)
  where
    ds = [formatValue val | (_, val) <- matches]
    pairs' = [(valueType val, mv) | (mv, val) <- matches] ++ pairs

prop_matchAndFollow_NoHelp (AnyArg s) pairs =
  getHelp (matchAndFollow s $ mkFollower pairs) === mempty



-- * Tests for matchShort

prop_matchShort_Matches (AnyChar c) =
  runParser (matchShort c) [['-', c]] === Right c

prop_matchShort_Finishes (AnyChar c) (AnyArg y) =
  runParser (matchShort c *> args) [['-', c], y] === Right [y]

prop_matchShort_Skips (AnyChar c) (AnyArg y) =
  runParser (matchShort c #> args) [y, ['-', c]] === Right [y]

prop_matchShort_FinishesInBundle (AnyChar c) (AnyChars cs) =
  runParser (matchShort c *> shorts) ['-':c:cs] === Right cs

prop_matchShort_SkipsInBundle (AnyChar c) (AnyChars cs) =
  not (c `elem` cs) ==>
  runParser (matchShort c #> shorts) [['-'] ++ cs ++ [c]] === Right cs

prop_matchShort_Empty (AnyChar c) =
  isLeft' $ runParser (matchShort c) []

prop_matchShort_OrElse (AnyChar c) x =
  runParser (matchShort c <|> orElse x) [] === Right x

prop_matchShort_NotMatches (AnyChar c) (AnyChar x) =
  x /= c ==>
  isLeft' $ runParser (matchShort c) [['-', x]]

prop_matchShort_MatchesBundle (AnyChars cs) =
  not (null cs) ==>
  runParser (traverse matchShort cs) ['-':cs] === Right cs

prop_matchShort_NoHelp (AnyChar c) =
  getHelp (matchShort c) === mempty


-- * Tests for utilities

prop_withHelp_MatchesMain builder =
  runParser (withHelp $ parser ex) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder

prop_withHelp_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  runParser p ["--help"] === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = withHelp $ parser ex

prop_withHelp_MatchesHelpMiddle builder (AnyArgs ys) =
  not ("--help" `member` consumes ex) ==>
  forAll (concat <$> (prefix $ blocks ex)) $ \pref ->
    runParser p (pref ++ ["--help"] ++ ys) === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = withHelp $ parser ex

prop_withHelp_AddsHelp builder =
  getHelp (withHelp $ parser ex) =/= getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_withHelp'_MatchesMain builder =
  runParser (withHelp' $ parser ex) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder

prop_withHelp'_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  runParser (withHelp' p) ["--help"] === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withHelp'_MatchesHelpMiddle builder (AnyArgs ys) =
  not ("--help" `member` consumes ex) ==>
  forAll (concat <$> (prefix $ blocks ex)) $ \pref ->
    runParser (withHelp' p) (pref ++ ["--help"] ++ ys)
    === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withHelp'_AddsNoHelp builder =
  getHelp (withHelp' $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_withSubHelp_MatchesMain builder =
  runParser (withSubHelp p) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  runParser (withSubHelp p) ["--help"] === (Right . Left . getHelp $ withHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp_MatchesHelpMiddle builder (AnyArgs ys) =
  not ("--help" `member` consumes ex) ==>
  forAll (concat <$> (prefix $ blocks ex)) $ \pref ->
    runParser (withSubHelp p) (pref ++ ["--help"] ++ ys)
    === (Right . Left . getHelp $ withHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp_ClearsHelp builder =
  getHelp (withSubHelp $ parser ex) === mempty
  where
    ex = buildGenericExample builder


prop_withSubHelp'_MatchesMain builder =
  runParser (withSubHelp' p) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp'_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  runParser (withSubHelp' p) ["--help"] === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp'_MatchesHelpMiddle builder (AnyArgs ys) =
  not ("--help" `member` consumes ex) ==>
  forAll (concat <$> (prefix $ blocks ex)) $ \pref ->
    runParser (withSubHelp' p) (pref ++ ["--help"] ++ ys)
    === (Right . Left $ getHelp p)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withSubHelp'_ClearsHelp builder =
  getHelp (withSubHelp' $ parser ex) === mempty
  where
    ex = buildGenericExample builder


prop_withVersion_MatchesMain s builder =
  runParser (withVersion s p) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withVersion_MatchesVersion s builder =
  not ("--version" `member` consumes ex) ==>
  runParser (withVersion s $ parser ex) ["--version"] === (Right $ Left s)
  where
    ex = buildGenericExample builder

prop_withVersion_AddsHelp s builder =
  getHelp (withVersion s $ parser ex) =/= getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_withVersion'_MatchesMain s builder =
  runParser (withVersion' s p) (inputs ex) === (Right . Right $ result ex)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_withVersion'_MatchesVersion s builder =
  not ("--version" `member` consumes ex) ==>
  runParser (withVersion' s $ parser ex) ["--version"] === (Right $ Left s)
  where
    ex = buildGenericExample builder

prop_withVersion'_AddsNoHelp s builder =
  getHelp (withVersion' s $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_beforeDashes_MatchesMain builder =
  runParser (beforeDashes $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

-- TODO: make this fail (when p consumes "--"). Right now it doesn't fail
-- because GenericExample is too limited, all its parsers finish without
-- waiting for EOF.
prop_beforeDashes_Matches builder =
  runParser (beforeDashes p) (inputs ex ++ ["--"]) === Right (result ex)
  where
    ex = buildGenericExample builder
    p = parser ex

prop_beforeDashes_Finishes builder (AnyArgs ys) =
  runParser (beforeDashes p *> args) (inputs ex ++ ["--"] ++ ys) === Right ys
  where
    ex = buildGenericExample builder
    p = parser ex

prop_beforeDashes_AddsNoHelp builder =
  getHelp (beforeDashes $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_quiet_Matches builder =
  runParser (quiet $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_quiet_AddsNoHelp builder =
  getHelp (quiet $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_eject_Matches b1 b2 =
  runParser (eject (parser ex1) (parser ex2)) (inputs ex1)
  === (Right . Right $ result ex1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

-- TODO: Also check that 'eject' can eject in the middle of a parse.

prop_eject_Ejects b1 b2 (AnyArgs ys) =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser (eject (parser ex1) (parser ex2)) (inputs ex2 ++ ys)
  === (Right . Left $ result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_eject_EjectsAfter b1 b2 (AnyArgs ys) =
  runParser (eject (parser ex1) (parser ex2)) (inputs ex1 ++ inputs ex2 ++ ys)
  === (Right . Left $ result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_eject_EjectsMiddle b1 b2 (AnyArgs ys) =
  consumes ex1 `disjoint` consumes ex2 ==>
  forAll (concat <$> prefix (blocks ex1)) $ \pref ->
    runParser (eject (parser ex1) (parser ex2)) (pref ++ inputs ex2 ++ ys)
    === (Right . Left $ result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_eject_EjectsLongAfter b1 (AnyArgs ys) b2 (AnyArgs zs) =
  not (any (`member` c2) ys) ==>
  runParser ((,) <$> eject p1 p2 <#> args) (i1 ++ ys ++ i2 ++ zs)
  === (Right (Left r2, ys))
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2
    r2 = result ex2
    c2 = consumes ex2

prop_eject_JoinsHelp b1 b2 =
  getHelp (eject p1 p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2



-- * Tests for manipulating help in 'Parser' objects.

prop_header_Matches s builder =
  runParser (header s $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_header_PrependsHeader s builder =
  getHelp (header s $ parser ex) === makeHeader s <> getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_footer_Matches s builder =
  runParser (footer s $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_footer_PrependsFooter s builder =
  getHelp (footer s $ parser ex) === makeFooter s <> getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_flagHelp_Matches fs desc builder =
  runParser (modify $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder
    modify = flagHelp (allForms fs) desc

prop_flagHelp_PrependsHelp fs desc builder =
  getHelp (flagHelp forms desc $ parser ex)
  === makeFlagHelp forms desc <> getHelp (parser ex)
  where
    ex = buildGenericExample builder
    forms = allForms fs

prop_flagHelp_IllegalForm (ChosenIllegal fs) desc builder =
  throwsError . formatHelp . getHelp . flagHelp (allForms fs) desc $ parser ex
  where
    ex = buildGenericExample builder


prop_paramHelp_Matches fs metavar desc builder =
  runParser (modify $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder
    modify = paramHelp (allForms fs) metavar desc

prop_paramHelp_PrependsHelp fs metavar desc builder =
  getHelp (paramHelp forms metavar desc $ parser ex)
  === makeParamHelp forms metavar desc <> getHelp (parser ex)
  where
    ex = buildGenericExample builder
    forms = allForms fs

prop_paramHelp_IllegalForm (ChosenIllegal fs) metavar desc builder =
  throwsError . formatHelp . getHelp
  . paramHelp (allForms fs) metavar desc $ parser ex
  where
    ex = buildGenericExample builder


prop_freeArgHelp_Matches metavar desc builder =
  runParser (modify $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder
    modify = freeArgHelp metavar desc

prop_freeArgHelp_PrependsHelp metavar desc builder =
  getHelp (freeArgHelp metavar desc $ parser ex)
  === makeFreeArgHelp metavar desc <> getHelp (parser ex)
  where
    ex = buildGenericExample builder


prop_multiParamHelp_Matches fs fh desc builder =
  runParser (modify $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder
    modify = multiParamHelp (allForms fs) fh desc

prop_multiParamHelp_PrependsHelp fs fh desc builder =
  getHelp (multiParamHelp forms fh desc $ parser ex)
  === makeMultiParamHelp forms fh desc <> getHelp (parser ex)
  where
    ex = buildGenericExample builder
    forms = allForms fs

prop_multiParamHelp_IllegalForm (ChosenIllegal fs) fh desc builder =
  throwsError . formatHelp . getHelp
  . multiParamHelp (allForms fs) fh desc $ parser ex
  where
    ex = buildGenericExample builder


prop_clearHelp_Matches builder =
  runParser (clearHelp $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_clearHelp_ClearsHelp builder =
  getHelp (clearHelp $ parser ex) === mempty
  where
    ex = buildGenericExample builder


prop_clearHeader_Matches builder =
  runParser (clearHeader $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_clearHeader_ClearsHeader builder =
  getHelp (clearHeader $ parser ex) === clearHelpHeader (getHelp $ parser ex)
  where
    ex = buildGenericExample builder


prop_clearFooter_Matches builder =
  runParser (clearFooter $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_clearFooter_ClearsFooter builder =
  getHelp (clearFooter $ parser ex) === clearHelpFooter (getHelp $ parser ex)
  where
    ex = buildGenericExample builder


prop_clearTable_Matches builder =
  runParser (clearTable $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_clearTable_ClearsTable builder =
  getHelp (clearTable $ parser ex) === clearHelpTable (getHelp $ parser ex)
  where
    ex = buildGenericExample builder


prop_sortTable_Matches builder =
  runParser (sortTable $ parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_sortTable_SortsTable builder =
  getHelp (sortTable $ parser ex) === sortHelpTable (getHelp $ parser ex)
  where
    ex = buildGenericExample builder



-- * Tests for IO functions

prop_runParserIO_Returns builder =
  runParserIO (parser ex) (inputs ex) `sameIO` return (result ex)
  where
    ex = buildGenericExample builder

prop_runParserIO_Dies builder (AnyArgs as) env =
  isLeft (runParser (parser ex) as) ==>
  diesWithoutStdout $ runTestIO' (runParserIO (parser ex) as) env
  where
    ex = buildGenericExample builder


prop_parseArgs_Returns builder s =
  runTestIO' (parseArgs (parser ex)) (TestEnv s (inputs ex))
  === (TestReturn (result ex), "")
  where
    ex = buildGenericExample builder

prop_parseArgs_Dies builder env@(TestEnv _ as) =
  isLeft (runParser (parser ex) as) ==>
  diesWithoutStdout $ runTestIO' (parseArgs $ parser ex) env
  where
    ex = buildGenericExample builder


prop_parseArgsWithHelp_Returns builder s =
  runTestIO' (parseArgsWithHelp (parser ex)) (TestEnv s (inputs ex))
  === (TestReturn (result ex), "")
  where
    ex = buildGenericExample builder

prop_parseArgsWithHelp_PrintsHelp builder s =
  not ("--help" `member` consumes ex) ==>
  runTestIO' (parseArgsWithHelp p) (TestEnv s ["--help"])
  === (TestExitSuccess, (formatHelp . getHelp $ withHelp p) ++ "\n")
  where
    ex = buildGenericExample builder
    p = parser ex

prop_parseArgsWithHelp_Dies builder env@(TestEnv _ as) =
  isLeft (runParser (withHelp $ parser ex) as) ==>
  diesWithoutStdout $ runTestIO' (parseArgsWithHelp $ parser ex) env
  where
    ex = buildGenericExample builder


prop_withHelpIO_MatchesMain builder =
  join (runParserIO (withHelpIO $ parser ex) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder

prop_withHelpIO_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  join (runParserIO (withHelpIO p) ["--help"]) `sameIO` do
    putStrLn . formatHelp . getHelp $ withHelp p
    exitSuccess
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withHelpIO_AddsHelp builder =
  getHelp (withHelpIO $ parser ex) === getHelp (withHelp $ parser ex)
  where
    ex = buildGenericIOExample builder


prop_withHelpIO'_MatchesMain builder =
  join (runParserIO (withHelpIO' $ parser ex) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder

prop_withHelpIO'_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  join (runParserIO (withHelpIO' p) ["--help"]) `sameIO` do
    putStrLn . formatHelp $ getHelp p
    exitSuccess
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withHelpIO'_AddsNoHelp builder =
  getHelp (withHelpIO' $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericIOExample builder


prop_withSubHelpIO_MatchesMain builder =
  join (runParserIO (withSubHelpIO $ parser ex) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder

prop_withSubHelpIO_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  join (runParserIO (withSubHelpIO p) ["--help"]) `sameIO` do
    putStrLn . formatHelp . getHelp $ withHelp p
    exitSuccess
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withSubHelpIO_ClearsHelp builder =
  getHelp (withSubHelpIO $ parser ex) === mempty
  where
    ex = buildGenericIOExample builder


prop_withSubHelpIO'_MatchesMain builder =
  join (runParserIO (withSubHelpIO' $ parser ex) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder

prop_withSubHelpIO'_MatchesHelp builder =
  not ("--help" `member` consumes ex) ==>
  join (runParserIO (withSubHelpIO' p) ["--help"]) `sameIO` do
    putStrLn . formatHelp $ getHelp p
    exitSuccess
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withSubHelpIO'_ClearsHelp builder =
  getHelp (withSubHelpIO' $ parser ex) === mempty
  where
    ex = buildGenericIOExample builder


prop_withVersionIO_MatchesMain s builder =
  join (runParserIO (withVersionIO s p) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withVersionIO_MatchesVersion s builder =
  not ("--version" `member` consumes ex) ==>
  join (runParserIO (withVersionIO s $ parser ex) ["--version"]) `sameIO` do
    putStrLn s
    exitSuccess
  where
    ex = buildGenericIOExample builder

prop_withVersionIO_AddsHelp s builder =
  getHelp (withVersionIO s $ parser ex) === getHelp (withVersion s $ parser ex)
  where
    ex = buildGenericIOExample builder


prop_withVersionIO'_MatchesMain s builder =
  join (runParserIO (withVersionIO' s p) (inputs ex)) `sameIO` result ex
  where
    ex = buildGenericIOExample builder
    p = parser ex

prop_withVersionIO'_MatchesVersion s builder =
  not ("--version" `member` consumes ex) ==>
  join (runParserIO (withVersionIO' s $ parser ex) ["--version"]) `sameIO` do
    putStrLn s
    exitSuccess
  where
    ex = buildGenericIOExample builder

prop_withVersionIO'_AddsNoHelp s builder =
  getHelp (withVersionIO' s $ parser ex) === getHelp (parser ex)
  where
    ex = buildGenericIOExample builder


-- * Tests for Functor

prop_fmap_MapsSuccess builder (Fun _ (f :: String -> String)) =
  runParser (fmap f p) as === fmap f (runParser p as)
  where
    ex = buildGenericExample builder
    p = parser ex
    as = inputs ex

prop_fmap_MapsAnyResult builder (AnyArgs as) (Fun _ (f :: String -> String)) =
  runParser (fmap f p) as === fmap f (runParser p as)
  where
    p = parser $ buildGenericExample builder

prop_fmap_AddsNoHelp builder (Fun _ (f :: String -> String)) =
  getHelp (fmap f p) === getHelp p
  where
    p = parser $ buildGenericExample builder


-- * Tests for FunctorFail

prop_fmapOrFail_MapsSuccess builder (Fun _ (f :: String -> String)) =
  runParser (fmapOrFail (Right . f) p) as === fmap f (runParser p as)
  where
    ex = buildGenericExample builder
    p = parser ex
    as = inputs ex

prop_fmapOrFail_MapsSuccessToFailure builder s =
  isLeft' $ runParser (fmapOrFail func (parser ex)) (inputs ex)
  where
    ex = buildGenericExample builder
    func _ = (Left s) :: Either String String

prop_fmapOrFail_MapsAnyToFailure builder s (AnyArgs as)=
  isLeft' $ runParser (fmapOrFail func (parser ex)) as
  where
    ex = buildGenericExample builder
    func _ = (Left s) :: Either String String

prop_fmapOrFail_AddsNoHelp builder (Fun _ f) =
  getHelp (fmapOrFail f p :: Parser String) === getHelp p
  where
    p = parser $ buildGenericExample builder



-- * Tests for Applicative

prop_pure_Matches (x :: String) =
  runParser (pure x) [] === Right x

prop_pure_Finishes (x :: String) (AnyArgs as) =
  runParser (pure x *> args) as === Right as

prop_pure_NotMatches (x :: String) (AnyArgs as) =
  not (null as) ==>
  isLeft' $ runParser (pure x) as

prop_pure_NoHelp (x :: String) =
  getHelp (pure x) === mempty


-- TODO: make this test fail by generating better GenericExample. Then fix by
-- requiring that the first parser finishes without waiting for EOF.
prop_ap_Matches b1 b2 =
  runParser ((,) <$> parser ex1 <*> parser ex2) (inputs ex1 ++ inputs ex2)
  === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_ap_MatchesFar b1 b2 (AnyArgs as) =
  not (any (`member` c2) as) ==>
  runParser (((,) <$> p1 <*> p2) <# args) (i1 ++ as ++ i2) === Right (r1, r2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2
    r1 = result ex1
    r2 = result ex2
    c2 = consumes ex2

prop_ap_JoinsHelp b1 b2 =
  getHelp ((,) <$> p1 <*> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2



-- * Tests for ApplicativeFail

prop_failA_Fails s (AnyArgs as) =
  isLeft' $ runParser (failA s :: Parser String) as

prop_failA_OrElse s (x :: String) =
  isLeft' $ runParser (failA s <|> orElse x) []

prop_failA_NoHelp s =
  getHelp (failA s :: Parser String) === mempty


-- * Test for Alternative and co.

prop_empty_Fails (AnyArgs as) =
  isLeft' $ runParser (empty :: Parser String) as

prop_empty_OrElse (x :: String) =
  runParser (empty <|> orElse x) [] === Right x

prop_empty_NoHelp =
  getHelp (empty :: Parser String) === mempty


prop_alternative_MatchesFirst b1 b2 =
  runParser (parser ex1 <|> parser ex2) (inputs ex1) === Right (result ex1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

-- TODO: make this fail by improving GenericExample. Then add missing
-- requirement: parser ex1 shouldn't finish immediately, and also one of the
-- parsers shouldn't accept EOF.
prop_alternative_MatchesSecond b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser (parser ex1 <|> parser ex2) (inputs ex2) === Right (result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_alternative_LeftEmpty builder =
  runParser (empty <|> parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_alternative_RightEmpty builder =
  runParser (parser ex <|> empty) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_alternative_JoinsHelp b1 b2 =
  getHelp (p1 <|> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


prop_leftAlternative_MatchesFirst b1 b2 =
  runParser (parser ex1 <-|> parser ex2) (inputs ex1) === Right (result ex1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_leftAlternative_MatchesSecond b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser (parser ex1 <-|> parser ex2) (inputs ex2) === Right (result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_leftAlternative_MatchesMiddle b1 b2 =
  consumes ex1 `disjoint` consumes ex2 && (not . null $ blocks ex1) ==>
  forAll (concat <$> properPrefix (blocks ex1)) $ \pref ->
    runParser (parser ex1 <-|> parser ex2) (pref ++ inputs ex2)
    === Right (result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_leftAlternative_LeftEmpty builder =
  runParser (empty <-|> parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_leftAlternative_RightEmpty builder =
  runParser (parser ex <-|> empty) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_leftAlternative_JoinsHelp b1 b2 =
  getHelp (p1 <-|> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


prop_rightAlternative_MatchesFirst b1 b2 =
  runParser (parser ex1 <|-> parser ex2) (inputs ex1) === Right (result ex1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_rightAlternative_MatchesSecond b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser (parser ex1 <|-> parser ex2) (inputs ex2) === Right (result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_rightAlternative_MatchesMiddle b1 b2 =
  consumes ex1 `disjoint` consumes ex2 && (not . null $ blocks ex2) ==>
  forAll (concat <$> properPrefix (blocks ex2)) $ \pref ->
    runParser (parser ex1 <|-> parser ex2) (pref ++ inputs ex1)
    === Right (result ex1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_rightAlternative_LeftEmpty builder =
  runParser (empty <|-> parser ex) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_rightAlternative_RightEmpty builder =
  runParser (parser ex <|-> empty) (inputs ex) === Right (result ex)
  where
    ex = buildGenericExample builder

prop_rightAlternative_JoinsHelp b1 b2 =
  getHelp (p1 <|-> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


-- * Tests for SelectiveParser

prop_eof_Empty =
  runParser eof [] === Right ()

prop_eof_Skips (AnyArgs as) =
  runParser (eof #> args) as === Right as

prop_eof_OrElse =
  runParser (eof $> True <|> orElse False) [] === Right True

prop_eof_NoHelp =
  getHelp eof === mempty


-- TODO: make this fail by improving GenericExample. Then fix by requiring that
-- the consumed languages be disjoint. Right now this should fail if e.g.  ex1
-- is 'many x' and ex2 is 'x'.
prop_parallel_DirectOrder b1 b2 =
  runParser ((,) <$> parser ex1 <#> parser ex2) (inputs ex1 ++ inputs ex2)
  === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_parallel_ReverseOrder b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser ((,) <$> parser ex1 <#> parser ex2) (inputs ex2 ++ inputs ex1)
  === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_parallel_DirectOrderFar b1 b2 (AnyArgs as) =
  not (any (`member` c2) as) ==>
  runParser (((,,) <$> p1 <#> p2) <#> args) (i1 ++ as ++ i2)
  === Right (r1, r2, as)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2
    r1 = result ex1
    r2 = result ex2
    c2 = consumes ex2

prop_parallel_ReverseOrderFar b1 b2 (AnyArgs as) =
  c1 `disjoint` c2 && not (any (`member` c1) as) ==>
  runParser (((,,) <$> p1 <#> p2) <#> args) (i2 ++ as ++ i1)
  === Right (r1, r2, as)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2
    r1 = result ex1
    r2 = result ex2
    c1 = consumes ex1
    c2 = consumes ex2

prop_parallel_Mix b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  forAll (concat <$> mix (blocks ex1) (blocks ex2)) $ \inputs ->
    runParser ((,) <$> parser ex1 <#> parser ex2) inputs
    === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_parallel_JoinsHelp b1 b2 =
  getHelp ((,) <$> p1 <#> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


prop_leftParallel_Matches b1 b2 =
  runParser ((,) <$> parser ex1 <-#> parser ex2) (inputs ex1 ++ inputs ex2)
  === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_leftParallel_NotMatchesReverse b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  isLeft $ runParser ((,) <$> p1 <-#> p2) (i2 ++ i1)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2

-- TODO: Make this fail by improving GenericExample (if parser ex1 accepts
-- EOF).
prop_leftParallel_MatchesInterrupting b1 b2 x =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser ((,) <$> (parser ex1 <|> orElse x) <-#> parser ex2) (inputs ex2)
  === Right (x, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_leftParallel_FinishesInterrupting b1 b2 x (AnyArg y) =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser (((,) <$> (p1 <|> orElse x) <-#> p2) *> args) (i2 ++ [y])
  === Right [y]
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i2 = inputs ex2

prop_leftParallel_JoinsHelp b1 b2 =
  getHelp ((,) <$> p1 <-#> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


prop_rightParallel_Matches b1 b2 =
  consumes ex1 `disjoint` consumes ex2 ==>
  runParser ((,) <$> parser ex1 <#-> parser ex2) (inputs ex2 ++ inputs ex1)
  === Right (result ex1, result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

-- TODO: make this fail by improving GenericExample (if ex2 accepts EOF).
prop_rightParallel_NotMatchesDirect b1 b2 =
  isLeft $ runParser ((,) <$> p1 <#-> p2) (i1 ++ i2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2

prop_rightParallel_MatchesInterrupting b1 b2 x =
  runParser ((,) <$> parser ex1 <#-> (parser ex2 <|> orElse x)) (inputs ex1)
  === Right (result ex1, x)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_rightParallel_FinishesInterrupting b1 b2 x (AnyArg y) =
  runParser (((,) <$> p1 <#-> (p2 <|> orElse x)) *> args) (i1 ++ [y])
  === Right [y]
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1

prop_rightParallel_JoinsHelp b1 b2 =
  getHelp ((,) <$> p1 <#-> p2) === getHelp p1 <> getHelp p2
  where
    p1 = parser $ buildGenericExample b1
    p2 = parser $ buildGenericExample b2


-- TODO: make this fail by improving GenericExample. Should only work if
-- @parser ex@ doesn't accept an empty input.
prop_many_MatchesOne builder =
  runParser (many $ parser ex) (inputs ex) === Right [result ex]
  where
    ex = buildGenericExample builder

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_many_MatchesZero builder =
  runParser (many $ parser ex) [] === Right []
  where
    ex = buildGenericExample builder

prop_many_AddsNoHelp builder =
  getHelp (many p) === getHelp p
  where
    p = parser $ buildGenericExample builder


-- TODO: make this fail by improving GenericExample. Should only work if
-- @parser ex@ doesn't accept an empty input.
prop_some_MatchesOne builder =
  runParser (some $ parser ex) (inputs ex) === Right [result ex]
  where
    ex = buildGenericExample builder

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_some_NotMatchesZero builder =
  isLeft' $ runParser (some $ parser ex) []
  where
    ex = buildGenericExample builder

prop_some_AddsNoHelp builder =
  getHelp (some p) === getHelp p
  where
    p = parser $ buildGenericExample builder


prop_optional_Matches builder =
  runParser (optional $ parser ex) (inputs ex) === (Right . Just $ result ex)
  where
    ex = buildGenericExample builder

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_optional_MatchesEmpty builder =
  runParser (optional $ parser ex) [] === Right Nothing
  where
    ex = buildGenericExample builder

prop_optional_AddsNoHelp builder =
  getHelp (optional p) === getHelp p
  where
    p = parser $ buildGenericExample builder


prop_between_Matches
  (AnyArg s) (NonNegative low) (NonNegative a) (NonNegative b) =
  runParser (between low high $ match s) (replicate x s)
  === Right (replicate x s)
  where
    x = low + a
    high = x + b

prop_between_TooMany
  (AnyArg s) (NonNegative low) (NonNegative a) (Positive b) =
  isLeft' $ runParser (between low high $ match s) (replicate x s)
  where
    high = low + a
    x = high + b

prop_between_TooFew
  (AnyArg s) (NonNegative x) (Positive a) (NonNegative b) =
  isLeft' $ runParser (between low high $ match s) (replicate x s)
  where
    low = x + a
    high = low + b

prop_between_NegativeLow (AnyArg s) low high =
  low < 0 ==>
  throwsError . toRaw $ between low high (match s)

prop_between_SmallHigh (AnyArg s) low high =
  high < low ==>
  throwsError . toRaw $ between low high (match s)

prop_between_AddsNoHelp builder (NonNegative low) (NonNegative a) =
  getHelp (between low (low + a) p) === getHelp p
  where
    p = parser $ buildGenericExample builder


prop_perm_Matches bs =
  mutuallyDisjoint cs ==>
  forAll (shuffle $ zip is rs) $ \pairs ->
    runParser (perm ps) (concat $ map fst pairs) === Right (map snd pairs)
  where
    es = map buildGenericExample bs
    ps = map parser es
    is = map inputs es
    rs = map result es
    cs = map consumes es

prop_perm_JoinsHelp bs =
  getHelp (perm ps) === foldMap getHelp ps
  where
    ps = map (parser . buildGenericExample) bs

prop_blah (_ :: Int, _ :: String) = True


-- TODO: Improve tests for 'many', 'some', 'between' by generating multiple
--       valid inputs for the same parser.
-- TODO: Try refactoring examples so that Example pieces are pattern-matched,
--       like it is done for 'Fun'.
-- TODO: Test parse failures for *Char and *Read.
-- TODO: Test that withVersion* can interrupt a parse in the middle.

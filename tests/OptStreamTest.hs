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
import Test.QuickCheck hiding (output)

import Options.OptStream
import Options.OptStream.Help
import Options.OptStream.IOOps
import Options.OptStream.Test.Helpers hiding (null, empty)
import Options.OptStream.Test.TestIO hiding (args)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "generic"
    [ testProperty "Matches" prop_Matches
    , testProperty "Finishes" prop_Finishes
    , testProperty "Skips" prop_Skips
    , testProperty "Empty" prop_Empty
    , testProperty "OrElse" prop_OrElse
    ]

  , testGroup "flag"
    [ testProperty "NotMatches"       prop_flag_NotMatches
    , testProperty "MatchesBundle"    prop_flag_MatchesBundle
    , testProperty "NotMatchesBundle" prop_flag_NotMatchesBundle
    , testProperty "Help"             prop_flag_Help
    , testProperty "NoHelp"           prop_flag_NoHelp
    , testProperty "EmptyForms"       prop_flag_EmptyForms
    , testProperty "IllegalForm"      prop_flag_IllegalForm
    ]

  , testGroup "param"
    [ testProperty "NotMatches"    prop_param_NotMatches
    , testProperty "MissingArg"    prop_param_MissingArg
    , testProperty "Help"          prop_param_Help
    , testProperty "NoHelp"        prop_param_NoHelp
    , testProperty "EmptyForms"    prop_param_EmptyForms
    , testProperty "IllegalForm"   prop_param_IllegalForm
    ]

  , testGroup "freeArg"
    [ testProperty "NotMatches" prop_freeArg_NotMatches
    , testProperty "Help"       prop_freeArg_Help
    , testProperty "NoHelp"     prop_freeArg_NoHelp
    ]

  , testGroup "anyArg"
    [ testProperty "Help"       prop_anyArg_Help
    , testProperty "NoHelp"     prop_anyArg_NoHelp
    ]

  , testGroup "multiParam"
    [ testProperty "NotMatches"  prop_multiParam_NotMatches
    , testProperty "NotEnough"   prop_multiParam_NotEnough
    , testProperty "Help"        prop_multiParam_Help
    , testProperty "NoHelp"      prop_multiParam_NoHelp
    , testProperty "EmptyForms"  prop_multiParam_EmptyForms
    , testProperty "IllegalForm" prop_multiParam_IllegalForm
    ]

  , testGroup "match"
    [ testProperty "NotMatches" prop_match_NotMatches
    , testProperty "NoHelp"     prop_match_NoHelp
    ]

  , testGroup "matchAndFollow"
    [ testProperty "NotMatches" prop_matchAndFollow_NotMatches
    , testProperty "NotEnough"  prop_matchAndFollow_NotEnough
    , testProperty "NoHelp"     prop_matchAndFollow_NoHelp
    ]

  , testGroup "matchShort"
    [ testProperty "FinishesInBundle" prop_matchShort_FinishesInBundle
    , testProperty "SkipsInBundle"    prop_matchShort_SkipsInBundle
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
    [ testProperty "Matches"     prop_many_Matches
    , testProperty "MatchesZero" prop_many_MatchesZero
    , testProperty "AddsNoHelp"  prop_many_AddsNoHelp
    ]

  , testGroup "some"
    [ testProperty "Matches"        prop_some_Matches
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



-- * Generic tests

prop_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser p i === Right o

prop_Finishes (AnyParser p_ p) (AnyArg x) =
  forAllExamples p_ $ \i o ->
  runParser (p *> args) (i ++ [x]) === Right [x]

prop_Skips (AnyParser p_ p) (AnyArg x) =
  not (x `member` consumes p_) ==>
  forAllExamples p_ $ \i _ ->
  runParser (p #> args) (x:i) === Right [x]

prop_Empty (AnyParser _ p) =
  isLeft' $ runParser p []

prop_OrElse (AnyParser _ p) x =
  runParser (p <|> orElse x) [] === Right x



-- * Tests for @flag*@

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

prop_freeArg_NotMatches help valueType metavar a =
  isLeft' $ runParser (mkFreeArg help valueType metavar) ['-':a]

prop_freeArg_Help desc valueType metavar =
  getHelp (mkFreeArg (WithHelp desc) valueType metavar)
  === makeFreeArgHelp metavar desc

prop_freeArg_NoHelp valueType metavar =
  getHelp (mkFreeArg WithoutHelp valueType metavar) === mempty



-- * Tests for @anyArg*@

prop_anyArg_Help desc valueType metavar =
  getHelp (toParser $ DescAnyArg valueType metavar (WithHelp desc))
  === makeFreeArgHelp metavar desc

prop_anyArg_NoHelp valueType metavar =
  getHelp (toParser $ DescAnyArg valueType metavar WithoutHelp) === mempty



-- * Tests for @multiParam*@

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

prop_match_NotMatches (AnyArg s) (AnyArg x) =
  x /= s ==>
  isLeft' $ runParser (match s) [x]

prop_match_NoHelp (AnyArg s) =
  getHelp (match s) === mempty



-- * Tests for matchAndFollow


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

prop_matchShort_FinishesInBundle (AnyChar c) (AnyChars cs) =
  runParser (matchShort c *> shorts) ['-':c:cs] === Right cs

prop_matchShort_SkipsInBundle (AnyChar c) (AnyChars cs) =
  not (c `elem` cs) ==>
  runParser (matchShort c #> shorts) [['-'] ++ cs ++ [c]] === Right cs

prop_matchShort_NotMatches (AnyChar c) (AnyChar x) =
  x /= c ==>
  isLeft' $ runParser (matchShort c) [['-', x]]

prop_matchShort_MatchesBundle (AnyChars cs) =
  not (null cs) ==>
  runParser (traverse matchShort cs) ['-':cs] === Right cs

prop_matchShort_NoHelp (AnyChar c) =
  getHelp (matchShort c) === mempty


-- * Tests for utilities

prop_withHelp_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withHelp p) i === (Right $ Right o)

prop_withHelp_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  runParser p' ["--help"] === (Right . Left $ getHelp p')
  where
    p' = withHelp p

prop_withHelp_MatchesHelpMiddle (AnyParser p_ p) (AnyArgs ys) =
  not ("--help" `member` consumes p_) ==>
  forAllEx p_ $ \ex ->
  forAll (prefix $ pieces ex) $ \pcs ->
  forAll (glue pcs) $ \i ->
  runParser p' (i ++ ["--help"] ++ ys) === (Right . Left $ getHelp p')
  where
    p' = withHelp p

prop_withHelp_AddsHelp (AnyParser _ p) =
  getHelp (withHelp p) =/= getHelp p


prop_withHelp'_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withHelp' p) i === (Right $ Right o)

prop_withHelp'_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  runParser (withHelp' p) ["--help"] === (Right . Left $ getHelp p)

prop_withHelp'_MatchesHelpMiddle (AnyParser p_ p) (AnyArgs ys) =
  not ("--help" `member` consumes p_) ==>
  forAllEx p_ $ \ex ->
  forAll (prefix $ pieces ex) $ \pcs ->
  forAll (glue pcs) $ \i ->
  runParser (withHelp' p) (i ++ ["--help"] ++ ys) === (Right . Left $ getHelp p)

prop_withHelp'_AddsNoHelp (AnyParser _ p) =
  getHelp (withHelp' p) === getHelp p


prop_withSubHelp_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withSubHelp p) i === (Right $ Right o)

prop_withSubHelp_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  runParser (withSubHelp p) ["--help"] === (Right . Left . getHelp $ withHelp p)

prop_withSubHelp_MatchesHelpMiddle (AnyParser p_ p) (AnyArgs ys) =
  not ("--help" `member` consumes p_) ==>
  forAllEx p_ $ \ex ->
  forAll (prefix $ pieces ex) $ \pcs ->
  forAll (glue pcs) $ \i ->
  runParser (withSubHelp p) (i ++ ["--help"] ++ ys)
  === (Right . Left . getHelp $ withHelp p)

prop_withSubHelp_ClearsHelp (AnyParser _ p) =
  getHelp (withSubHelp p) === mempty


prop_withSubHelp'_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withSubHelp' p) i === (Right $ Right o)

prop_withSubHelp'_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  runParser (withSubHelp' p) ["--help"] === (Right . Left $ getHelp p)

prop_withSubHelp'_MatchesHelpMiddle (AnyParser p_ p) (AnyArgs ys) =
  not ("--help" `member` consumes p_) ==>
  forAllEx p_ $ \ex ->
  forAll (prefix $ pieces ex) $ \pcs ->
  forAll (glue pcs) $ \i ->
  runParser (withSubHelp' p) (i ++ ["--help"] ++ ys)
  === (Right . Left $ getHelp p)

prop_withSubHelp'_ClearsHelp (AnyParser _ p) =
  getHelp (withSubHelp' p) === mempty


prop_withVersion_MatchesMain s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withVersion s p) i === (Right $ Right o)

prop_withVersion_MatchesVersion s (AnyParser p_ p) =
  not ("--version" `member` consumes p_) ==>
  runParser (withVersion s p) ["--version"] === (Right $ Left s)

prop_withVersion_AddsHelp s (AnyParser _ p) =
  getHelp (withVersion s p) =/= getHelp p


prop_withVersion'_MatchesMain s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (withVersion' s p) i === (Right $ Right o)

prop_withVersion'_MatchesVersion s (AnyParser p_ p) =
  not ("--version" `member` consumes p_) ==>
  runParser (withVersion' s p) ["--version"] === (Right $ Left s)

prop_withVersion'_AddsNoHelp s (AnyParser _ p) =
  getHelp (withVersion' s p) === getHelp p


prop_beforeDashes_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (beforeDashes p) i === Right o

-- TODO: make this fail (when p consumes' "--"). Right now it doesn't fail
-- because GenericExample is too limited, all its parsers finish without
-- waiting for EOF.
prop_beforeDashes_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (beforeDashes p) (i ++ ["--"]) === Right o

prop_beforeDashes_Finishes (AnyParser p_ p) (AnyArgs xs) =
  forAllExamples p_ $ \i _ ->
  runParser (beforeDashes p *> args) (i ++ ["--"] ++ xs) === Right xs

prop_beforeDashes_AddsNoHelp (AnyParser _ p) =
  getHelp (beforeDashes p) === getHelp p


prop_quiet_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (quiet p) i === Right o

prop_quiet_AddsNoHelp (AnyParser _ p) =
  getHelp (quiet p) === getHelp p


prop_eject_Matches (AnyParser p1_ p1) (AnyParser _ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  runParser (eject p1 p2) i1 === (Right $ Right o1)

prop_eject_Ejects (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs xs) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p2_ $ \i2 o2 ->
  runParser (eject p1 p2) (i2 ++ xs) === (Right $ Left o2)

prop_eject_EjectsAfter (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs xs) =
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser (eject p1 p2) (i1 ++ i2 ++ xs) === (Right $ Left o2)

prop_eject_EjectsMiddle (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs xs) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllEx p1_ $ \ex1 ->
  forAllExamples p2_ $ \i2 o2 ->
  forAll (prefix $ pieces ex1) $ \pcs ->
  forAll (glue pcs) $ \i1 ->
  runParser (eject p1 p2) (i1 ++ i2 ++ xs) === (Right $ Left o2)

prop_eject_EjectsLongAfter
  (AnyParser p1_ p1) (AnyArgs xs) (AnyParser p2_ p2) (AnyArgs ys) =
  not (any (`member` consumes p2_) xs) ==>
  forAllExamples p1_ $ \i1 _ ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> eject p1 p2 <#> args) (i1 ++ xs ++ i2 ++ ys)
  === (Right (Left o2, xs))

prop_eject_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp (eject p1 p2) === getHelp p1 <> getHelp p2



-- * Tests for manipulating help in 'Parser' objects.

prop_header_Matches s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (header s p) i === Right o

prop_header_PrependsHeader s (AnyParser _ p) =
  getHelp (header s p) === makeHeader s <> getHelp p


prop_footer_Matches s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (footer s p) i === Right o

prop_footer_PrependsFooter s (AnyParser _ p) =
  getHelp (footer s p) === makeFooter s <> getHelp p

prop_flagHelp_Matches fs desc (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (flagHelp (allForms fs) desc p) i === Right o

prop_flagHelp_PrependsHelp fs desc (AnyParser p_ p) =
  getHelp (flagHelp forms desc p) === makeFlagHelp forms desc <> getHelp p
  where
    forms = allForms fs

prop_flagHelp_IllegalForm (ChosenIllegal fs) desc (AnyParser p_ p) =
  throwsError . formatHelp . getHelp $ flagHelp (allForms fs) desc p


prop_paramHelp_Matches fs metavar desc (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (paramHelp (allForms fs) metavar desc p) i === Right o

prop_paramHelp_PrependsHelp fs metavar desc (AnyParser _ p) =
  getHelp (paramHelp forms metavar desc p)
  === makeParamHelp forms metavar desc <> getHelp p
  where
    forms = allForms fs

prop_paramHelp_IllegalForm (ChosenIllegal fs) metavar desc (AnyParser _ p) =
  throwsError . formatHelp . getHelp $ paramHelp (allForms fs) metavar desc p


prop_freeArgHelp_Matches metavar desc (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (freeArgHelp metavar desc p) i === Right o

prop_freeArgHelp_PrependsHelp metavar desc (AnyParser _ p) =
  getHelp (freeArgHelp metavar desc p)
  === makeFreeArgHelp metavar desc <> getHelp p


prop_multiParamHelp_Matches fs fh desc (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (multiParamHelp (allForms fs) fh desc p) i === Right o

prop_multiParamHelp_PrependsHelp fs fh desc (AnyParser _ p) =
  getHelp (multiParamHelp forms fh desc p)
  === makeMultiParamHelp forms fh desc <> getHelp p
  where
    forms = allForms fs

prop_multiParamHelp_IllegalForm (ChosenIllegal fs) fh desc (AnyParser _ p) =
  throwsError . formatHelp . getHelp $ multiParamHelp (allForms fs) fh desc p


prop_clearHelp_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (clearHelp p) i === Right o

prop_clearHelp_ClearsHelp (AnyParser _ p) =
  getHelp (clearHelp p) === mempty


prop_clearHeader_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (clearHeader p) i === Right o

prop_clearHeader_ClearsHeader (AnyParser _ p) =
  getHelp (clearHeader p) === clearHelpHeader (getHelp p)


prop_clearFooter_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (clearFooter p) i === Right o

prop_clearFooter_ClearsFooter (AnyParser _ p) =
  getHelp (clearFooter p) === clearHelpFooter (getHelp p)


prop_clearTable_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (clearTable p) i === Right o

prop_clearTable_ClearsTable (AnyParser _ p) =
  getHelp (clearTable p) === clearHelpTable (getHelp p)


prop_sortTable_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (sortTable p) i === Right o

prop_sortTable_SortsTable (AnyParser _ p) =
  getHelp (sortTable p) === sortHelpTable (getHelp p)



-- * Tests for IO functions

prop_runParserIO_Returns (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParserIO p i `sameIO` return o

prop_runParserIO_Dies (AnyParser _ p) (AnyArgs as) env =
  isLeft (runParser p as) ==>
  diesWithoutStdout $ runTestIO' (runParserIO p as) env


prop_parseArgs_Returns (AnyParser p_ p) s =
  forAllExamples p_ $ \i o ->
  runTestIO' (parseArgs p) (TestEnv s i) === (TestReturn o, "")

prop_parseArgs_Dies (AnyParser _ p) env@(TestEnv _ as) =
  isLeft (runParser p as) ==>
  diesWithoutStdout $ runTestIO' (parseArgs p) env


prop_parseArgsWithHelp_Returns (AnyParser p_ p) s =
  forAllExamples p_ $ \i o ->
  runTestIO' (parseArgsWithHelp p) (TestEnv s i) === (TestReturn o, "")

prop_parseArgsWithHelp_PrintsHelp (AnyParser p_ p) s =
  not ("--help" `member` consumes p_) ==>
  runTestIO' (parseArgsWithHelp p) (TestEnv s ["--help"])
  === (TestExitSuccess, (formatHelp . getHelp $ withHelp p) ++ "\n")

prop_parseArgsWithHelp_Dies (AnyParser _ p) env@(TestEnv _ as) =
  isLeft (runParser (withHelp p) as) ==>
  diesWithoutStdout $ runTestIO' (parseArgsWithHelp p) env


prop_withHelpIO_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withHelpIO $ return <$> p) i) `sameIO` return o

prop_withHelpIO_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  join (runParserIO (withHelpIO $ return <$> p) ["--help"]) `sameIO` do
    putStrLn . formatHelp . getHelp $ withHelp p
    exitSuccess

prop_withHelpIO_AddsHelp (AnyParser _ p) =
  getHelp (withHelpIO p') === getHelp (withHelp p')
  where
    p' = return <$> p :: Parser (TestIO String)


prop_withHelpIO'_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withHelpIO' $ return <$> p) i) `sameIO` return o

prop_withHelpIO'_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  join (runParserIO (withHelpIO' $ return <$> p) ["--help"]) `sameIO` do
    putStrLn . formatHelp $ getHelp p
    exitSuccess

prop_withHelpIO'_AddsNoHelp (AnyParser _ p) =
  getHelp (withHelpIO' p') === getHelp p'
  where
    p' = return <$> p :: Parser (TestIO String)


prop_withSubHelpIO_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withSubHelpIO $ return <$> p) i) `sameIO` return o

prop_withSubHelpIO_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  join (runParserIO (withSubHelpIO $ return <$> p) ["--help"]) `sameIO` do
    putStrLn . formatHelp . getHelp $ withHelp p
    exitSuccess

prop_withSubHelpIO_ClearsHelp (AnyParser _ p) =
  getHelp (withSubHelpIO p') === mempty
  where
    p' = return <$> p :: Parser (TestIO String)

prop_withSubHelpIO'_MatchesMain (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withSubHelpIO' $ return <$> p) i) `sameIO` return o

prop_withSubHelpIO'_MatchesHelp (AnyParser p_ p) =
  not ("--help" `member` consumes p_) ==>
  join (runParserIO (withSubHelpIO' $ return <$> p) ["--help"]) `sameIO` do
    putStrLn . formatHelp $ getHelp p
    exitSuccess

prop_withSubHelpIO'_ClearsHelp (AnyParser _ p) =
  getHelp (withSubHelpIO' p') === mempty
  where
    p' = return <$> p :: Parser (TestIO String)


prop_withVersionIO_MatchesMain s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withVersionIO s $ return <$> p) i) `sameIO` return o

prop_withVersionIO_MatchesVersion s (AnyParser p_ p) =
  not ("--version" `member` consumes p_) ==>
  join (runParserIO (withVersionIO s $ return <$> p) ["--version"]) `sameIO` do
    putStrLn s
    exitSuccess

prop_withVersionIO_AddsHelp s (AnyParser _ p) =
  getHelp (withVersionIO s p') === getHelp (withVersion s p')
  where
    p' = return <$> p :: Parser (TestIO String)


prop_withVersionIO'_MatchesMain s (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  join (runParserIO (withVersionIO' s $ return <$> p) i) `sameIO` return o

prop_withVersionIO'_MatchesVersion s (AnyParser p_ p) =
  not ("--version" `member` consumes p_) ==>
  join (runParserIO (withVersionIO' s $ return <$> p) ["--version"]) `sameIO` do
    putStrLn s
    exitSuccess

prop_withVersionIO'_AddsNoHelp s (AnyParser _ p) =
  getHelp (withVersionIO' s p') === getHelp p'
  where
    p' = return <$> p :: Parser (TestIO String)


-- * Tests for Functor

prop_fmap_MapsSuccess (AnyParser p_ p) (Fun _ (f :: String -> String)) =
  forAllExamples p_ $ \i _ ->
  runParser (fmap f p) i === fmap f (runParser p i)

prop_fmap_MapsAnyResult
  (AnyParser p_ p) (AnyArgs as) (Fun _ (f :: String -> String)) =
  forAllExamples p_ $ \i _ ->
  runParser (fmap f p) i === fmap f (runParser p i)

prop_fmap_AddsNoHelp (AnyParser _ p) (Fun _ (f :: String -> String)) =
  getHelp (fmap f p) === getHelp p


-- * Tests for FunctorFail

prop_fmapOrFail_MapsSuccess (AnyParser p_ p) (Fun _ (f :: String -> String)) =
  forAllExamples p_ $ \i o ->
  runParser (fmapOrFail (Right . f) p) i === fmap f (runParser p i)

prop_fmapOrFail_MapsSuccessToFailure (AnyParser p_ p) s =
  forAllExamples p_ $ \i o ->
  isLeft' $ runParser (fmapOrFail func p) i
  where
    func _ = (Left s) :: Either String String

prop_fmapOrFail_MapsAnyToFailure (AnyParser _ p) s (AnyArgs as)=
  isLeft' $ runParser (fmapOrFail func p) as
  where
    func _ = (Left s) :: Either String String

prop_fmapOrFail_AddsNoHelp (AnyParser _ p) (Fun _ f) =
  getHelp (fmapOrFail f p :: Parser String) === getHelp p



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
prop_ap_Matches (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> p1 <*> p2) (i1 ++ i2) === Right (o1, o2)

prop_ap_MatchesFar (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs as) =
  not (any (`member` consumes p2_) as) ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser (((,) <$> p1 <*> p2) <# args) (i1 ++ as ++ i2) === Right (o1, o2)

prop_ap_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp ((,) <$> p1 <*> p2) === getHelp p1 <> getHelp p2



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


prop_alternative_MatchesFirst (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  runParser (p1 <|> p2) i1 === Right o1

-- TODO: make this fail by improving GenericExample. Then add missing
-- requirement: parser ex1 shouldn't finish immediately, and also one of the
-- parsers shouldn't accept EOF.
prop_alternative_MatchesSecond (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p2_ $ \i2 o2 ->
  consumes p1_ `disjoint` consumes p2_ ==>
  runParser (p1 <|> p2) i2 === Right o2

prop_alternative_LeftEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (empty <|> p) i === Right o

prop_alternative_RightEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (p <|> empty) i === Right o

prop_alternative_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp (p1 <|> p2) === getHelp p1 <> getHelp p2


prop_leftAlternative_MatchesFirst (AnyParser p1_ p1) (AnyParser _ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  runParser (p1 <-|> p2) i1 === Right o1

prop_leftAlternative_MatchesSecond (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p2_ $ \i2 o2 ->
  consumes p1_ `disjoint` consumes p2_ ==>
  runParser (p1 <-|> p2) i2 === Right o2

prop_leftAlternative_MatchesMiddle (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllEx p1_ $ \ex1 -> (not . null $ pieces ex1) ==>
  forAllExamples p2_ $ \i2 o2 ->
  forAll (properPrefix $ pieces ex1) $ \pcs ->
  forAll (glue pcs) $ \i1 ->
  runParser (p1 <-|> p2) (i1 ++ i2) === Right o2

prop_leftAlternative_LeftEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (empty <-|> p) i === Right o

prop_leftAlternative_RightEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (p <-|> empty) i === Right o

prop_leftAlternative_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp (p1 <-|> p2) === getHelp p1 <> getHelp p2


prop_rightAlternative_MatchesFirst (AnyParser p1_ p1) (AnyParser _ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  runParser (p1 <|-> p2) i1 === Right o1

prop_rightAlternative_MatchesSecond (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p2_ $ \i2 o2 ->
  runParser (p1 <|-> p2) i2 === Right o2

prop_rightAlternative_MatchesMiddle (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllEx p2_ $ \ex2 -> (not . null $ pieces ex2) ==>
  forAll (properPrefix $ pieces ex2) $ \pcs ->
  forAll (glue pcs) $ \i2 ->
  runParser (p1 <|-> p2) (i2 ++ i1) === Right o1

prop_rightAlternative_LeftEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (empty <|-> p) i === Right o

prop_rightAlternative_RightEmpty (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (p <|-> empty) i === Right o

prop_rightAlternative_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp (p1 <|-> p2) === getHelp p1 <> getHelp p2


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
prop_parallel_DirectOrder (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> p1 <#> p2) (i1 ++ i2) === Right (o1, o2)

prop_parallel_ReverseOrder (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> p1 <#> p2) (i2 ++ i1) === Right (o1, o2)

prop_parallel_DirectOrderFar
  (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs as) =
  not (any (`member` consumes p2_) as) ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser (((,,) <$> p1 <#> p2) <#> args) (i1 ++ as ++ i2)
  === Right (o1, o2, as)

prop_parallel_ReverseOrderFar
  (AnyParser p1_ p1) (AnyParser p2_ p2) (AnyArgs as) =
  c1 `disjoint` c2 && not (any (`member` c1) as) ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser (((,,) <$> p1 <#> p2) <#> args) (i2 ++ as ++ i1)
  === Right (o1, o2, as)
  where
    c1 = consumes p1_
    c2 = consumes p2_

prop_parallel_Mix (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllEx p1_ $ \ex1 ->
  forAllEx p2_ $ \ex2 ->
  forAll (mix (pieces ex1) (pieces ex2)) $ \pcs ->
  forAll (glue pcs) $ \i ->
  runParser ((,) <$> p1 <#> p2) i === Right (output ex1, output ex2)

prop_parallel_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp ((,) <$> p1 <#> p2) === getHelp p1 <> getHelp p2


prop_leftParallel_Matches (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> p1 <-#> p2) (i1 ++ i2) === Right (o1, o2)

prop_leftParallel_NotMatchesReverse (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  isLeft $ runParser ((,) <$> p1 <-#> p2) (i2 ++ i1)

-- TODO: Make this fail by improving GenericExample (if parser ex1 accepts
-- EOF).
prop_leftParallel_MatchesInterrupting (AnyParser p1_ p1) (AnyParser p2_ p2) x =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> (p1 <|> orElse x) <-#> p2) i2 === Right (x, o2)

prop_leftParallel_FinishesInterrupting
  (AnyParser p1_ p1) (AnyParser p2_ p2) x (AnyArg y) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p2_ $ \i2 _ ->
  runParser (((,) <$> (p1 <|> orElse x) <-#> p2) *> args) (i2 ++ [y])
  === Right [y]

prop_leftParallel_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp ((,) <$> p1 <-#> p2) === getHelp p1 <> getHelp p2


prop_rightParallel_Matches (AnyParser p1_ p1) (AnyParser p2_ p2) =
  consumes p1_ `disjoint` consumes p2_ ==>
  forAllExamples p1_ $ \i1 o1 ->
  forAllExamples p2_ $ \i2 o2 ->
  runParser ((,) <$> p1 <#-> p2) (i2 ++ i1) === Right (o1, o2)

-- TODO: make this fail by improving GenericExample (if ex2 accepts EOF).
prop_rightParallel_NotMatchesDirect (AnyParser p1_ p1) (AnyParser p2_ p2) =
  forAllExamples p1_ $ \i1 _ ->
  forAllExamples p2_ $ \i2 _ ->
  isLeft $ runParser ((,) <$> p1 <#-> p2) (i1 ++ i2)

prop_rightParallel_MatchesInterrupting (AnyParser p1_ p1) (AnyParser _ p2) x =
  forAllExamples p1_ $ \i1 o1 ->
  runParser ((,) <$> p1 <#-> (p2 <|> orElse x)) i1 === Right (o1, x)

prop_rightParallel_FinishesInterrupting
  (AnyParser p1_ p1) (AnyParser _ p2) x (AnyArg y) =
  forAllExamples p1_ $ \i1 _ ->
  runParser (((,) <$> p1 <#-> (p2 <|> orElse x)) *> args) (i1 ++ [y])
  === Right [y]

prop_rightParallel_JoinsHelp (AnyParser _ p1) (AnyParser _ p2) =
  getHelp ((,) <$> p1 <#-> p2) === getHelp p1 <> getHelp p2


-- TODO: make this fail by improving GenericExample. Should only work if
-- @parser ex@ doesn't accept an empty input.
prop_many_Matches (AnyParser p_ p) =
  forAllShrink (listOf $ arbitraryEx p_) (shrinkList shrinkEx) $ \exs ->
  forAll (traverse (glue . pieces) exs) $ \is ->
  runParser (many p) (concat is) === (Right $ map output exs)

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_many_MatchesZero (AnyParser _ p) =
  runParser (many p) [] === Right []

prop_many_AddsNoHelp (AnyParser _ p) =
  getHelp (many p) === getHelp p


-- TODO: make this fail by improving GenericExample. Should only work if
-- @parser ex@ doesn't accept an empty input.
prop_some_Matches (AnyParser p_ p) =
  forAllShrink (listOf $ arbitraryEx p_) (shrinkList shrinkEx) $ \exs ->
  (not $ null exs) ==>
  forAll (traverse (glue . pieces) exs) $ \is ->
  runParser (some p) (concat is) === (Right $ map output exs)

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_some_NotMatchesZero (AnyParser _ p) =
  isLeft' $ runParser (some p) []

prop_some_AddsNoHelp (AnyParser _ p) =
  getHelp (some p) === getHelp p


prop_optional_Matches (AnyParser p_ p) =
  forAllExamples p_ $ \i o ->
  runParser (optional p) i === (Right $ Just o)

-- TODO: make this fail by improving GenericExample. This should only work if
-- @parser ex@ doesn't accept an empty input.
prop_optional_MatchesEmpty (AnyParser _ p) =
  runParser (optional p) [] === Right Nothing

prop_optional_AddsNoHelp (AnyParser _ p) =
  getHelp (optional p) === getHelp p


prop_between_Matches
  (AnyParser p_ p) (NonNegative low) (NonNegative a) (NonNegative b) =
  forAllShrink
    (sequenceA . replicate x $ arbitraryEx p_)
    (shrinkElements shrinkEx)
    $ \exs ->
  forAll (traverse (glue . pieces) exs) $ \is ->
  runParser (between low high p) (concat is) === (Right $ map output exs)
  where
    x = low + a
    high = x + b

prop_between_TooMany
  (AnyParser p_ p) (NonNegative low) (NonNegative a) (Positive b) =
  forAllShrink
    (sequenceA . replicate x $ arbitraryEx p_)
    (shrinkElements shrinkEx)
    $ \exs ->
  forAll (traverse (glue . pieces) exs) $ \is -> 
  isLeft' $ runParser (between low high p) (concat is)
  where
    high = low + a
    x = high + b

prop_between_TooFew
  (AnyParser p_ p) (NonNegative x) (Positive a) (NonNegative b) =
  forAllShrink
    (sequenceA . replicate x $ arbitraryEx p_)
    (shrinkElements shrinkEx)
    $ \exs ->
  forAll (traverse (glue . pieces) exs) $ \is ->
  isLeft' $ runParser (between low high p) (concat is)
  where
    low = x + a
    high = low + b

prop_between_NegativeLow (AnyArg s) low high =
  low < 0 ==>
  throwsError . toRaw $ between low high (match s)

prop_between_SmallHigh (AnyArg s) low high =
  high < low ==>
  throwsError . toRaw $ between low high (match s)

prop_between_AddsNoHelp (AnyParser _ p) (NonNegative low) (NonNegative a) =
  getHelp (between low (low + a) p) === getHelp p


prop_perm_Matches ps_ =
  mutuallyDisjoint (map consumes ps_) ==>
  forAllExs ps_ $ \exs ->
  forAll (shuffle exs) $ \exs' ->
  forAll (traverse (glue . pieces) exs') $ \is' ->
  runParser (perm $ map toParser ps_) (concat is') === Right (map output exs')

prop_perm_JoinsHelp ps_ =
  getHelp (perm ps) === foldMap getHelp ps
  where
    ps = map toParser ps_

-- TODO: Test parse failures for *Char and *Read.
-- TODO: Test that withVersion* can interrupt a parse in the middle.

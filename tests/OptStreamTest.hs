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
import Options.OptStream.Test.Helpers hiding (null)
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
    ]

  , testGroup "matchAndFollow"
    [ testProperty "Matches"    prop_matchAndFollow_Matches
    , testProperty "Finishes"   prop_matchAndFollow_Finishes
    , testProperty "Skips"      prop_matchAndFollow_Skips
    , testProperty "Empty"      prop_matchAndFollow_Empty
    , testProperty "OrElse"     prop_matchAndFollow_OrElse
    , testProperty "NotMatches" prop_matchAndFollow_NotMatches
    , testProperty "NotEnough"  prop_matchAndFollow_NotEnough
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
    ]

  , testGroup "withHelp"
    [ testProperty "MatchesMain" prop_withHelp_MatchesMain
    , testProperty "MatchesHelp" prop_withHelp_MatchesHelp
    , testProperty "AddsHelp"    prop_withHelp_AddsHelp
    ]

  , testGroup "withHelp'"
    [ testProperty "MatchesMain" prop_withHelp'_MatchesMain
    , testProperty "MatchesHelp" prop_withHelp'_MatchesHelp
    , testProperty "AddsNoHelp"  prop_withHelp'_AddsNoHelp
    ]

  , testGroup "withSubHelp"
    [ testProperty "MatchesMain" prop_withSubHelp_MatchesMain
    , testProperty "MatchesHelp" prop_withSubHelp_MatchesHelp
    , testProperty "ClearsHelp"  prop_withSubHelp_ClearsHelp
    ]

  , testGroup "withSubHelp'"
    [ testProperty "MatchesMain" prop_withSubHelp'_MatchesMain
    , testProperty "MatchesHelp" prop_withSubHelp'_MatchesHelp
    , testProperty "ClearsrHelp" prop_withSubHelp'_ClearsHelp
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
  disjoint (consumes ex1) (consumes ex2) ==>
  runParser (eject (parser ex1) (parser ex2)) (inputs ex2 ++ ys)
  === (Right . Left $ result ex2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2

prop_eject_EjectsAfter b1 b2 (AnyArgs ys) =
  runParser (eject p1 p2) (i1 ++ i2 ++ ys) === (Right . Left $ r2)
  where
    ex1 = buildGenericExample b1
    ex2 = buildGenericExample b2
    p1 = parser ex1
    p2 = parser ex2
    i1 = inputs ex1
    i2 = inputs ex2
    r2 = result ex2

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


-- TODO: Test parse failures for *Char and *Read.
-- TODO: Test that withHelp et al. can interrupt a parse in the middle.

-- TODO: (?) test that atomic option parsers can be matched in any order with
--       <#> as long as they have non-intersecting sets of option forms. Also
--       check that free arguments can be squished in arbitrarily between them.

{-|
Module      : Options.OptStream.Test.Helpers
Copyright   : (c) Dan Shved, 2022
License     : BSD-3
Maintainer  : danshved@gmail.com
Stability   : experimental

Various helpers for testing "Options.OptStream" with QuickCheck. Contains
multiple 'Arbitrary' instances that generate 'Parser' objects and corresponding
command line examples for them to parse.
-}

{-# LANGUAGE DeriveGeneric #-}
module Options.OptStream.Test.Helpers where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Either (isLeft, isRight)
import Data.Functor (($>))
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Prelude hiding (null)
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , Property
  , Testable
  , choose
  , counterexample
  , elements
  , forAll
  , forAllShrink
  , frequency
  , genericShrink
  , ioProperty
  , listOf
  , oneof
  , property
  , shrinkList
  , shuffle
  , suchThat
  , (===)
  , (.&&.)
  )
import qualified Prelude as P

import Options.OptStream
import Options.OptStream.IOOps
import Options.OptStream.Test.TestIO


-- * ArgLanguage

-- | Represents the set of all possible arguments that could be consumed by
-- some Parser in a test example (not counting arguments consumed by
-- followers). These can be united, intersected, checked for emptiness and for
-- membership.
newtype ArgLanguage = ArgLanguage [Chunk]
  deriving Show

instance Semigroup ArgLanguage where
  (<>) = union

instance Monoid ArgLanguage where
  mempty = empty

empty :: ArgLanguage
empty = ArgLanguage []

-- | A set consisting of one given string.
singleton :: String -> ArgLanguage
singleton s = ArgLanguage [Singleton s]

-- | A set consisting of all strings that have the given prefix.
withPrefix :: String -> ArgLanguage
withPrefix s = ArgLanguage [WithPrefix s]

-- | The set consisting of all strings not starting with @\'-\'@.
freeArgs :: ArgLanguage
freeArgs = ArgLanguage [FreeArgs]

null :: ArgLanguage -> Bool
null (ArgLanguage []) = True
null _ = False

member :: String -> ArgLanguage -> Bool
member s l = not $ singleton s `disjoint` l

intersect :: ArgLanguage -> ArgLanguage -> ArgLanguage
intersect (ArgLanguage xs) (ArgLanguage ys) = ArgLanguage
  $ catMaybes [x `intersectChunks` y | x <- xs, y <- ys]

union :: ArgLanguage -> ArgLanguage -> ArgLanguage
union (ArgLanguage xs) (ArgLanguage ys) = ArgLanguage $ xs ++ ys

disjoint :: ArgLanguage -> ArgLanguage -> Bool
disjoint a b = null $ a `intersect` b

mutuallyDisjoint :: [ArgLanguage] -> Bool
mutuallyDisjoint [] = True
mutuallyDisjoint (x:[]) = True
mutuallyDisjoint (x:y:ys)
  | x `disjoint` y = mutuallyDisjoint $ (x `union` y):ys
  | otherwise = False


data Chunk
  = Singleton String
    -- ^ The set consisting of one string.
  | WithPrefix String
    -- ^ The set of all strings having a certain prefix.
  | FreeArgs
    -- ^ The set of all strings not starting with '-'.
  deriving Show

intersectChunks :: Chunk -> Chunk -> Maybe Chunk
intersectChunks (Singleton a) (Singleton b)
  | a == b = Just $ Singleton a
  | otherwise = Nothing

intersectChunks (WithPrefix a) (WithPrefix b)
  | a `isPrefixOf` b = Just $ WithPrefix b
  | b `isPrefixOf` a = Just $ WithPrefix a
  | otherwise = Nothing

intersectChunks FreeArgs FreeArgs = Just FreeArgs

intersectChunks (Singleton s) (WithPrefix p)
  | p `isPrefixOf` s = Just $ Singleton s
  | otherwise = Nothing
intersectChunks p@(WithPrefix _) e@(Singleton _) = intersectChunks e p

intersectChunks (Singleton ('-':_)) FreeArgs = Nothing
intersectChunks (Singleton s) FreeArgs = Just $ Singleton s
intersectChunks f@FreeArgs e@(Singleton _) = intersectChunks e f

intersectChunks (WithPrefix "") FreeArgs = Just FreeArgs
intersectChunks (WithPrefix ('-':_)) FreeArgs = Nothing
intersectChunks (WithPrefix p) FreeArgs = Just $ WithPrefix p
intersectChunks f@FreeArgs p@(WithPrefix _) = intersectChunks p f



-- * QuickCheck helpers

isLeft' :: (Show a, Show b) => Either a b -> Property
isLeft' x = counterexample (prefix ++ showsPrec 11 x "") res
  where
    res = isLeft x
    prefix = if res then "isLeft " else "not $ isLeft "

isRight' :: (Show a, Show b) => Either a b -> Property
isRight' x = counterexample (prefix ++ showsPrec 11 x "") res
  where
    res = isRight x
    prefix = if res then "isRight " else "not $ isRight "

throwsError :: a -> Property
throwsError a = ioProperty $ isErrorCall <$> try (evaluate a)
  where
    isErrorCall :: Either ErrorCall a -> Bool
    isErrorCall = isLeft

-- | Mix two lists, preserving the original order of both.
mix :: [a] -> [a] -> Gen [a]
mix as bs = construct as bs <$> shuffle ((as $> False) ++ (bs $> True))
  where
    construct [] [] [] = []
    construct (a:as) bs (False:xs) = a:construct as bs xs
    construct as (b:bs) (True:xs) = b:construct as bs xs

-- | Take a random prefix of a list.
prefix :: [a] -> Gen [a]
prefix as = do
  l <- choose (0, length as)
  return $ take l as

-- | Take a random prefix of a list that doesn't coincide with the full list.
-- Will throw an 'error' if the list is empty.
properPrefix :: [a] -> Gen [a]
properPrefix [] = error "properPrefix of an empty list"
properPrefix as = do
  l <- choose (0, length as - 1)
  return $ take l as



-- * Helpers for TestIO

instance Arbitrary TestEnv where
  arbitrary = TestEnv <$> arbitrary <*> listOf arbitraryArg
  shrink = genericShrink

isDie' :: Show a => TestResult a -> Property
isDie' x = counterexample (prefix ++ showsPrec 11 x "") res
  where
    res = isDie x
    prefix = if res then "isDie " else "not $ isDie "

diesWithoutStdout :: Show a => (TestResult a, String) -> Property
diesWithoutStdout (res, stdout) = isDie' res .&&. stdout === ""

sameIO :: (Eq a, Show a) => TestIO a -> TestIO a -> Property
sameIO x y = property $ \env -> runTestIO x env === runTestIO y env


-- * Producing atomic parsers for testing


-- | Represents an arbitrary character other than '-'.
newtype NotDash = NotDash Char
  deriving Show

instance Arbitrary NotDash where
  arbitrary = NotDash <$> arbitrary `suchThat` (/= '-')
  shrink (NotDash c) = [NotDash c' | c' <- shrink c, c' /= '-']


arbitraryShort :: Gen OptionForm
arbitraryShort = do
  c <- arbitrary `suchThat` (/= '-')
  return ['-', c]

shrinkShort :: OptionForm -> [OptionForm]
shrinkShort ['-', c] | c /= '-' = [['-', c'] | c' <- shrink c, c' /= '-']
shrinkShort _ = []

arbitraryLong :: Gen OptionForm
arbitraryLong = do
  s <- oneof [arbitrary `suchThat` (not . P.null), pure "help", pure "version"]
  return $ '-':'-':s

shrinkLong :: OptionForm -> [OptionForm]
shrinkLong ('-':'-':s@(_:_)) = ['-':'-':s' | s' <- shrink s, not $ P.null s']
shrinkLong _ = []

arbitraryLegal :: Gen OptionForm
arbitraryLegal = oneof [arbitraryShort, arbitraryLong]

shrinkLegal :: OptionForm -> [OptionForm]
shrinkLegal s@('-':'-':c:_)
  | c /= '-' = ['-', c]:shrinkLong s
  | otherwise = shrinkLong s
shrinkLegal s@('-':c:[])
  | c /= '-' = shrinkShort s
shrinkLegal _ = []

arbitraryLegals :: Gen [OptionForm]
arbitraryLegals = (:) <$> arbitraryLegal <*> listOf arbitraryLegal

shrinkLegals :: [OptionForm] -> [[OptionForm]]
shrinkLegals fs = filter (not . P.null) $ shrinkList shrinkLegal fs

-- TODO: improve distribution to include things like '-', '--' or '-abc'.
arbitraryIllegal :: Gen OptionForm
arbitraryIllegal = arbitrary `suchThat` (not . isLegalOptionForm)

shrinkIllegal :: OptionForm -> [OptionForm]
shrinkIllegal s = filter (not . isLegalOptionForm) $ shrink s

-- | Represents a list of legal option forms with one of them selected.
data Forms = Forms [OptionForm] OptionForm [OptionForm]
  deriving Show

allForms :: Forms -> [OptionForm]
allForms (Forms as b cs) = as ++ [b] ++ cs

chosenForm :: Forms -> OptionForm
chosenForm (Forms _ b _) = b

arbitraryForms :: Gen OptionForm -> Gen Forms
arbitraryForms arbitraryChosen = Forms
  <$> listOf arbitraryLegal
  <*> arbitraryChosen
  <*> listOf arbitraryLegal

shrinkForms :: (OptionForm -> [OptionForm]) -> Forms -> [Forms]
shrinkForms shrinkChosen (Forms as b cs)
    =  [Forms as' b cs | as' <- shrinkList shrinkLegal as]
    ++ [Forms as b' cs | b' <- shrinkChosen b]
    ++ [Forms as b cs' | cs' <- shrinkList shrinkLegal cs]

chooseLeft :: Forms -> [Forms]
chooseLeft (Forms (a:as) _ cs) = [Forms as a cs]
chooseLeft _ = []

chooseRight :: Forms -> [Forms]
chooseRight (Forms as _ (c:cs)) = [Forms as c cs]
chooseRight _ = []

instance Arbitrary Forms where
  arbitrary = arbitraryForms arbitraryLegal
  shrink fs = shrinkForms shrinkLegal fs ++ chooseLeft fs ++ chooseRight fs



-- | Represents a list of option forms where all of them are legal except one.
-- The illegal form is also the selected one.
newtype FormsI = ChosenIllegal Forms
  deriving Show

instance Arbitrary FormsI where
  arbitrary = ChosenIllegal <$> arbitraryForms arbitraryIllegal
  shrink (ChosenIllegal fs) =
    [ChosenIllegal fs' | fs' <- shrinkForms shrinkIllegal fs]


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


-- | Type of a value to be parsed with a parser like 'param' or 'freeArg'.
-- Represents a choice within the family of related parsers, e.g. 'param' vs
-- 'paramRead' vs 'paramChar'.
data ValueType
  = TypeString
  | TypeReadInt
  | TypeChar
  deriving (Show, Generic)

instance Arbitrary ValueType where
  arbitrary = elements [TypeString, TypeReadInt, TypeChar]

  shrink TypeString = []
  shrink _ = [TypeString]


-- | Represents a test value to be parsed with e.g. 'param', 'freeArg', or
-- 'next'.
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

arbitraryValue :: ValueType -> Gen Value
arbitraryValue TypeString = ValueString <$> arbitraryArg
arbitraryValue TypeReadInt = ValueReadInt <$> arbitrary
arbitraryValue TypeChar = ValueChar <$> arbitraryChar

instance Arbitrary Value where
  arbitrary = arbitrary >>= arbitraryValue

  shrink x@(ValueString _) = genericShrink x
  shrink x = ValueString (formatValue x):genericShrink x


-- | Represents a choice between flags with and without bundling, e.g. 'flag'
-- vs 'flagSep'.
data Bundling
  = WithoutBundling
  | WithBundling
  deriving Show

instance Arbitrary Bundling where
  arbitrary = elements [WithoutBundling, WithBundling]

  shrink WithBundling = [WithoutBundling]
  shrink _ = []


-- | Synonym for better readability.
type Metavar = String


-- TODO: Reorder params of mk* to more closely resemble underlying functions,
--       i.e. move HelpChoice to the end.

-- | Makes an arbitrary @flag*@ parser for testing.
mkFlag :: HelpChoice -> Bundling -> [OptionForm] -> Parser ()
mkFlag WithoutHelp WithoutBundling opts = flagSep' opts
mkFlag WithoutHelp WithBundling opts = flag' opts
mkFlag (WithHelp desc) WithoutBundling opts = flagSep opts desc
mkFlag (WithHelp desc) WithBundling opts = flag opts desc

-- | Makes an arbitrary @param*@ parser for testing.
mkParam :: HelpChoice -> ValueType -> [OptionForm] -> Metavar -> Parser String
mkParam WithoutHelp TypeString opts mv = param' opts mv
mkParam WithoutHelp TypeReadInt opts mv =
  show <$> (paramRead' opts mv :: Parser Int)
mkParam WithoutHelp TypeChar opts mv = (:[]) <$> paramChar' opts mv
mkParam (WithHelp desc) TypeString opts mv = param opts mv desc
mkParam (WithHelp desc) TypeReadInt opts mv =
  show <$> (paramRead opts mv desc :: Parser Int)
mkParam (WithHelp desc) TypeChar opts mv =
  (:[]) <$> paramChar opts mv desc

-- | Makes an arbitrary @freeArg*@ parser for testing.
mkFreeArg :: HelpChoice -> ValueType -> Metavar -> Parser String
mkFreeArg WithoutHelp TypeString mv = freeArg' mv
mkFreeArg WithoutHelp TypeReadInt mv = show <$> (freeArgRead' mv :: Parser Int)
mkFreeArg WithoutHelp TypeChar mv = (:[]) <$> freeArgChar' mv
mkFreeArg (WithHelp desc) TypeString mv = freeArg mv desc
mkFreeArg (WithHelp desc) TypeReadInt mv =
  show <$> (freeArgRead mv desc :: Parser Int)
mkFreeArg (WithHelp desc) TypeChar mv = (:[]) <$> freeArgChar mv desc

-- | Makes an arbitrary @multiParam*@ parser for testing.
mkMultiParam :: HelpChoice -> [OptionForm] -> Follower a -> Parser a
mkMultiParam WithoutHelp opts f = multiParam' opts f
mkMultiParam (WithHelp desc) opts f = multiParam opts f desc

-- | Makes an arbitrary @next*@ follower for testing.
mkNext :: ValueType -> Metavar -> Follower String
mkNext TypeString mv = next mv
mkNext TypeReadInt mv = show <$> (nextRead mv :: Follower Int)
mkNext TypeChar mv = (:[]) <$> nextChar mv

-- | Makes an arbitrary Follower for testing (using only <*>, not >>=).
mkFollower :: [(ValueType, Metavar)] -> Follower [String]
mkFollower = traverse (uncurry mkNext)



-- * Producing command line examples for testing


-- | Arbitrary string interesting for command line testing. Produces roughly
-- 50/50 strings starting with '-' and with other characters.
arbitraryArg :: Gen String
arbitraryArg = oneof
  [ arbitrary
  , oneof
    [ ("-" ++) <$> arbitrary
    , ("--" ++) <$> oneof
      [ arbitrary
      , pure "help"
      , pure "version"
      ]
    ]
  ]

-- | Arbitrary character interesting for command line testing. Produces '-' at
-- a reasonable rate, but not too often so that (listOf arbitraryChar) would
-- still produce lists free of '-' from time to time.
arbitraryChar :: Gen Char
arbitraryChar = frequency
  [ (3, arbitrary)
  , (1, pure '-')
  ]


-- | Represents an arbitrary command line argument. Can be any string, but the
-- distribution is skewed to produce more examples starting with @-@ and @--@.
newtype AnyArg = AnyArg String
  deriving (Show, Generic)

instance Arbitrary AnyArg where
  arbitrary = AnyArg <$> arbitraryArg
  shrink = genericShrink


-- | Represents an arbitrary list of command line arguments like 'AnyArg'.
newtype AnyArgs = AnyArgs [String]
  deriving (Show, Generic)

instance Arbitrary AnyArgs where
  arbitrary = AnyArgs <$> listOf arbitraryArg
  shrink = genericShrink


-- | Represents an arbitrary character interesting for command line testing.
newtype AnyChar = AnyChar Char
  deriving (Show, Generic)

instance Arbitrary AnyChar where
  arbitrary = AnyChar <$> arbitraryChar
  shrink = genericShrink


-- | Represents an arbitrary list of characters like 'AnyChar'
newtype AnyChars = AnyChars [Char]
  deriving (Show, Generic)

instance Arbitrary AnyChars where
  arbitrary = AnyChars <$> listOf arbitraryChar
  shrink = genericShrink




-- * Generating parsers.

-- | Precedence of function application.
appPrec :: Int
appPrec = 10

-- | Precedence of <$> and <$
fmapPrec :: Int
fmapPrec = 4

-- | "Precedence" of type annotations (::). Probably a heretic thing to say
-- since :: is not an operator, but pretending like it has precedence 0
-- produces satisfactory outputs for our Show instances.
typePrec :: Int
typePrec = 0

-- | Raw representation for "a thing that can be shown".
type ShowP = Int -> ShowS

-- | Helper to call ShowP objects.
showP :: Int -> ShowP -> ShowS
showP = flip id

-- TODO: try to make one 'appS' to rule them all using typeclass magic.

appS :: Show a => String -> a -> ShowP
appS f a p
  = showParen (appPrec < p)
  $ showString (f ++ " ")
  . showsPrec (appPrec + 1) a

appS2 :: (Show a, Show b) => String -> a -> b -> ShowP
appS2 f a b p
  = showParen (appPrec < p)
  $ showString (f ++ " ")
  . showsPrec (appPrec + 1) a
  . showString " "
  . showsPrec (appPrec + 1) b

appS3 :: (Show a, Show b, Show c) => String -> a -> b -> c -> ShowP
appS3 f a b c p
  = showParen (appPrec < p)
  $ showString (f ++ " ")
  . showsPrec (appPrec + 1) a
  . showString " "
  . showsPrec (appPrec + 1) b
  . showString " "
  . showsPrec (appPrec + 1) c

fmapS :: String -> String -> ShowP -> ShowP
fmapS f op showRHS p
  = showParen (fmapPrec < p)
  $ showString (f ++ " " ++ op ++ " ")
  . showRHS (fmapPrec + 1)

fmapS2 :: (Show a, Show b) => String -> String -> a -> String -> b -> ShowP
fmapS2 f op1 a op2 b p
  = showParen (fmapPrec < p)
  $ showString (f ++ " " ++ op1 ++ " ")
  . showsPrec (fmapPrec + 1) a
  . showString (" " ++ op2 ++ " ")
  . showsPrec (fmapPrec + 1) b

typeS :: String -> ShowP -> ShowP
typeS t showVal p
  = showParen (typePrec < p)
  $ showVal (typePrec + 1)
  . showString (" :: " ++ t)


data NextDesc = DescNext ValueType Metavar
  deriving (Generic)

toNext :: NextDesc -> Follower String
toNext (DescNext TypeString mv) = next mv
toNext (DescNext TypeReadInt mv) = show <$> (nextRead mv :: Follower Int)
toNext (DescNext TypeChar mv) = (:[]) <$> nextChar mv

instance Show NextDesc where
  showsPrec p (DescNext TypeString mv) = showP p $ appS "next" mv
  showsPrec p (DescNext TypeReadInt mv) = showP p
    $ fmapS "show" "<$>" $ typeS "Follower Int" $ appS "nextRead" mv
  showsPrec p (DescNext TypeChar mv) = showP p
    $ fmapS "(:[])" "<$>" $ appS "nextChar" mv

instance Arbitrary NextDesc where
  arbitrary = DescNext <$> arbitrary <*> arbitrary
  shrink = genericShrink


-- | Rerresents a 'Follower' that could reasonably be built by a user.
data FollowerDesc = DescFollower [NextDesc]
  deriving Generic

toFollower :: FollowerDesc -> Follower String
toFollower (DescFollower []) = pure ""
toFollower (DescFollower [desc]) = toNext desc
toFollower (DescFollower descs) = concat <$> sequenceA (map toNext descs)

instance Show FollowerDesc where
  showsPrec p (DescFollower []) = showP p $ appS "pure" "\"\""
  showsPrec p (DescFollower [desc]) = showsPrec p desc
  showsPrec p (DescFollower descs) = showP p $
    fmapS "concat" "<$>" $ appS "sequenceA" descs

instance Arbitrary FollowerDesc where
  arbitrary = DescFollower <$> arbitrary
  shrink = genericShrink


-- TODO: Think if this can be made nicer by making this a GADT, e.g.  having
--       'DescMatch' be a @ParserDesc String@ and 'DescFlag' be a @ParserDesc
--       ()@, etc.  Also think if there's anything to be gained from it. Also,
--       this could be a fun rabbit hole with followers, leading to something
--       like @ParserDesc (t1, (t2, (..., tn))@.
-- | Represents a 'Parser' that could reasonably be built by a user.
data ParserDesc
  = DescMatch String
  | DescMAF String FollowerDesc
  | DescMatchShort Char
  | DescFlag Bundling [OptionForm] HelpChoice
  | DescParam ValueType [OptionForm] Metavar HelpChoice
  | DescMultiParam [OptionForm] FollowerDesc HelpChoice
  | DescFreeArg ValueType Metavar HelpChoice
  | DescAnyArg ValueType Metavar HelpChoice
  | DescAp ParserDesc ParserDesc
  deriving (Generic)

-- TODO: deprecate mk* functions and use the below everywhere instead.
toParser :: ParserDesc -> Parser String
toParser (DescMatch s) = match s
toParser (DescMAF s fd) = matchAndFollow s (toFollower fd)
toParser (DescMatchShort c) = (:[]) <$> matchShort c

toParser (DescFlag WithoutBundling fs WithoutHelp) = "" <$ flagSep' fs
toParser (DescFlag WithoutBundling fs (WithHelp d)) = "" <$ flagSep fs d
toParser (DescFlag WithBundling fs WithoutHelp) = "" <$ flag' fs
toParser (DescFlag WithBundling fs (WithHelp d)) = "" <$ flag fs d

toParser (DescParam TypeString fs mv WithoutHelp) = param' fs mv
toParser (DescParam TypeString fs mv (WithHelp d)) = param fs mv d
toParser (DescParam TypeReadInt fs mv WithoutHelp) =
  show <$> (paramRead' fs mv :: Parser Int)
toParser (DescParam TypeReadInt fs mv (WithHelp d)) =
  show <$> (paramRead fs mv d :: Parser Int)
toParser (DescParam TypeChar fs mv WithoutHelp) = (:[]) <$> paramChar' fs mv
toParser (DescParam TypeChar fs mv (WithHelp d)) = (:[]) <$> paramChar fs mv d

toParser (DescMultiParam fs fd WithoutHelp) = multiParam' fs (toFollower fd)
toParser (DescMultiParam fs fd (WithHelp d)) = multiParam fs (toFollower fd) d

toParser (DescFreeArg TypeString mv WithoutHelp) = freeArg' mv
toParser (DescFreeArg TypeString mv (WithHelp d)) = freeArg mv d
toParser (DescFreeArg TypeReadInt mv WithoutHelp) =
  show <$> (freeArgRead' mv :: Parser Int)
toParser (DescFreeArg TypeReadInt mv (WithHelp d)) =
  show <$> (freeArgRead mv d :: Parser Int)
toParser (DescFreeArg TypeChar mv WithoutHelp) = (:[]) <$> freeArgChar' mv
toParser (DescFreeArg TypeChar mv (WithHelp d)) = (:[]) <$> freeArgChar mv d

toParser (DescAnyArg TypeString mv WithoutHelp) = anyArg' mv
toParser (DescAnyArg TypeString mv (WithHelp d)) = anyArg mv d
toParser (DescAnyArg TypeReadInt mv WithoutHelp) =
  show <$> (anyArgRead' mv :: Parser Int)
toParser (DescAnyArg TypeReadInt mv (WithHelp d)) =
  show <$> (anyArgRead mv d :: Parser Int)
toParser (DescAnyArg TypeChar mv WithoutHelp) = (:[]) <$> anyArgChar' mv
toParser (DescAnyArg TypeChar mv (WithHelp d)) = (:[]) <$> anyArgChar mv d

toParser (DescAp p1 p2) = (++) <$> toParser p1 <*> toParser p2


consumes :: ParserDesc -> ArgLanguage
consumes (DescMatch s) = singleton s
consumes (DescMAF s _) = singleton s
consumes (DescMatchShort c) = withPrefix ['-', c]
consumes (DescFlag WithoutBundling fs _) = mconcat $ map singleton fs
consumes (DescFlag WithBundling fs _) = mconcat $ map withPrefix fs
consumes (DescParam _ fs _ _) = mconcat $ map withPrefix fs
consumes (DescMultiParam fs _ _) = mconcat $ map singleton fs
consumes (DescFreeArg _ _ _) = freeArgs
consumes (DescAnyArg _ _ _) = withPrefix ""
consumes (DescAp p1 p2) = consumes p1 `union` consumes p2


instance Show ParserDesc where
  showsPrec p (DescMatch s) = showP p $ appS "match" s
  showsPrec p (DescMAF s fd) = showP p $ appS2 "matchAndFollow" s fd

  showsPrec p (DescMatchShort c) = showP p
    $ fmapS "(:[])" "<$>" $ appS "matchShort" c

  showsPrec p (DescFlag WithoutBundling fs WithoutHelp) = showP p
    $ fmapS "\"\"" "<$" $ appS "flagSep'" fs
  showsPrec p (DescFlag WithoutBundling fs (WithHelp d)) = showP p
    $ fmapS "\"\"" "<$" $ appS2 "flagSep" fs d
  showsPrec p (DescFlag WithBundling fs WithoutHelp) = showP p
    $ fmapS "\"\"" "<$" $ appS "flag'" fs
  showsPrec p (DescFlag WithBundling fs (WithHelp d)) = showP p
    $ fmapS "\"\"" "<$" $ appS2 "flag" fs d

  showsPrec p (DescParam TypeString fs mv WithoutHelp) = showP p
    $ appS2 "param'" fs mv
  showsPrec p (DescParam TypeString fs mv (WithHelp d)) = showP p
    $ appS3 "param" fs mv d
  showsPrec p (DescParam TypeReadInt fs mv WithoutHelp) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS2 "paramRead'" fs mv
  showsPrec p (DescParam TypeReadInt fs mv (WithHelp d)) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS3 "paramRead" fs mv d
  showsPrec p (DescParam TypeChar fs mv WithoutHelp) = showP p
    $ fmapS "(:[])" "<$>" $ appS2 "paramChar'" fs mv
  showsPrec p (DescParam TypeChar fs mv (WithHelp d)) = showP p
    $ fmapS "(:[])" "<$>" $ appS3 "paramChar" fs mv d

  showsPrec p (DescMultiParam fs fd WithoutHelp) = showP p
    $ appS2 "multiParam'" fs fd
  showsPrec p (DescMultiParam fs fd (WithHelp d)) = showP p
    $ appS3 "multiParam" fs fd d

  showsPrec p (DescFreeArg TypeString mv WithoutHelp) = showP p
    $ appS "freeArg'" mv
  showsPrec p (DescFreeArg TypeString mv (WithHelp d)) = showP p
    $ appS2 "freeArg" mv d
  showsPrec p (DescFreeArg TypeReadInt mv WithoutHelp) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS "freeArgRead'" mv
  showsPrec p (DescFreeArg TypeReadInt mv (WithHelp d)) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS2 "freeArgRead" mv d
  showsPrec p (DescFreeArg TypeChar mv WithoutHelp) = showP p
    $ fmapS "(:[])" "<$>" $ appS "freeArgChar'" mv
  showsPrec p (DescFreeArg TypeChar mv (WithHelp d)) = showP p
    $ fmapS "(:[])" "<$>" $ appS2 "freeArgChar" mv d

  showsPrec p (DescAnyArg TypeString mv WithoutHelp) = showP p
    $ appS "anyArg'" mv
  showsPrec p (DescAnyArg TypeString mv (WithHelp d)) = showP p
    $ appS2 "anyArg" mv d
  showsPrec p (DescAnyArg TypeReadInt mv WithoutHelp) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS "anyArgRead'" mv
  showsPrec p (DescAnyArg TypeReadInt mv (WithHelp d)) = showP p
    $ fmapS "show" "<$>" $ typeS "Parser Int" $ appS2 "anyArgRead" mv d
  showsPrec p (DescAnyArg TypeChar mv WithoutHelp) = showP p
    $ fmapS "(:[])" "<$>" $ appS "anyArgChar'" mv
  showsPrec p (DescAnyArg TypeChar mv (WithHelp d)) = showP p
    $ fmapS "(:[])" "<$>" $ appS2 "anyArgChar" mv d

  showsPrec p (DescAp p1 p2) = showP p
    $ fmapS2 "(++)" "<$>" p1 "<*>" p2


instance Arbitrary ParserDesc where
  -- Frequencies are are adjusted in order to have a roughly 33%-33%-33% split
  -- between atomic block parsers, atomic short flag parsers, and compound
  -- parsers.
  arbitrary = frequency
    [ (1, DescMatch <$> arbitraryArg)
    , (1, DescMAF <$> arbitraryArg <*> arbitrary)
    , (5, DescMatchShort <$> arbitraryChar)
    , (1, DescFlag <$> arbitrary <*> arbitraryLegals <*> arbitrary)
    , (1, DescParam <$> arbitrary <*> arbitraryLegals <*> arbitrary <*> arbitrary)
    , (1, DescMultiParam <$> arbitraryLegals <*> arbitrary <*> arbitrary)
    , (1, DescFreeArg <$> arbitrary <*> arbitrary <*> arbitrary)
    , (1, DescAnyArg <$> arbitrary <*> arbitrary <*> arbitrary)
    , (5, DescAp <$> arbitrary <*> arbitrary)
    ]

  shrink (DescMatch s) = [DescMatch s' | s' <- shrink s]

  shrink (DescMAF s f)
    =  [DescMAF s' f | s' <- shrink s]
    ++ [DescMAF s f' | f' <- shrink f]
    ++ [DescMatch s]

  shrink (DescMatchShort c)
    =  [DescMatchShort c' | c' <- shrink c]
    ++ [DescMatch ['-', c]]

  shrink (DescFlag b fs h)
    =  [DescFlag b' fs h | b' <- shrink b]
    ++ [DescFlag b fs' h | fs' <- shrinkLegals fs]
    ++ [DescFlag b fs h' | h' <- shrink h]
    ++ [DescMatch f | f <- fs]
    ++ [DescMatchShort c | ['-', c] <- fs]

  shrink (DescParam vt fs mv h)
    =  [DescParam vt' fs mv h | vt' <- shrink vt]
    ++ [DescParam vt fs' mv h | fs' <- shrinkLegals fs]
    ++ [DescParam vt fs mv' h | mv' <- shrink mv]
    ++ [DescParam vt fs mv h' | h' <- shrink h]
    ++ [DescMatch f | f <- fs]
    ++ [DescMAF f (DescFollower [DescNext vt mv]) | f <- fs]
    ++ [DescFlag WithoutBundling fs h]

  shrink (DescMultiParam fs fd h)
    =  [DescMultiParam fs' fd h | fs' <- shrinkLegals fs]
    ++ [DescMultiParam fs fd' h | fd' <- shrink fd]
    ++ [DescMultiParam fs fd h' | h' <- shrink h]
    ++ [DescMatch f | f <- fs]
    ++ [DescMAF f fd | f <- fs]
    ++ [DescFlag WithoutBundling fs h]
    ++ (toPar fd)
    where
      toPar (DescFollower ((DescNext vt mv):_)) = [DescParam vt fs mv h]
      toPar _ = []

  shrink (DescAnyArg vt mv h)
    =  [DescAnyArg vt' mv h | vt' <- shrink vt]
    ++ [DescAnyArg vt mv' h | mv' <- shrink mv]
    ++ [DescAnyArg vt mv h' | h' <- shrink h]

  shrink (DescFreeArg vt mv h)
    =  [DescFreeArg vt' mv h | vt' <- shrink vt]
    ++ [DescFreeArg vt mv' h | mv' <- shrink mv]
    ++ [DescFreeArg vt mv h' | h' <- shrink h]
    ++ [DescAnyArg vt mv h]

  shrink (DescAp l r)
    =  [DescAp l' r | l' <- shrink l]
    ++ [DescAp l r' | r' <- shrink r]
    ++ [l, r]


-- | Convenience datatype wrapping a parser together with its description.
-- Meant to be used as a function argument for QuickCheck properties.
data AnyParser = AnyParser ParserDesc (Parser String)

instance Show AnyParser where
  showsPrec p (AnyParser desc _) = showsPrec p desc

instance Arbitrary AnyParser where
  arbitrary = fmap (\d -> AnyParser d (toParser d)) arbitrary
  shrink (AnyParser d _) = [AnyParser d' (toParser d') | d' <- shrink d]


-- | Represents an input example for a parser that's expected to succeed.
data Example
  = ExMatch String String
  | ExMatchShort Char String
  | ExMAF String [Value]
  | ExParamShort String Value
  | ExParamLong String Value
  | ExAny Value
  | ExFree Value
  | ExAp Example Example
  deriving Show

data Piece
  = PieceBlock [String]
  | PieceShort Char
  deriving Show

pieces :: Example -> [Piece]
pieces (ExMatch i _) = [PieceBlock [i]]
pieces (ExMatchShort c _) = [PieceShort c]
pieces (ExMAF s vs) = [PieceBlock $ s:map formatValue vs]
pieces (ExParamShort s v) = [PieceBlock [s ++ formatValue v]]
pieces (ExParamLong s v) = [PieceBlock [s ++ "=" ++ formatValue v]]
pieces (ExAny v) = [PieceBlock [formatValue v]]
pieces (ExFree v) = [PieceBlock [formatValue v]]
pieces (ExAp l r) = pieces l ++ pieces r

glue :: [Piece] -> Gen [String]
glue [] = pure []
glue ((PieceBlock ss):ps) = (ss ++) <$> glue ps
glue ((PieceShort c):ps) = doGlue [c] ps where
  doGlue :: [Char] -> [Piece] -> Gen [String]
  doGlue cs ((PieceShort c'):ps') = oneof
    [ doGlue (c':cs) ps'
    , (('-':reverse cs):) <$> doGlue [c'] ps'
    ]
  doGlue cs ps' = (('-':reverse cs):) <$> glue ps'

input :: Example -> Gen [String]
input = glue . pieces

output :: Example -> String
output (ExMatch _ o) = o
output (ExMatchShort _ o) = o
output (ExMAF _ vs) = concat $ map formatValue vs
output (ExParamShort _ v) = formatValue v
output (ExParamLong _ v) = formatValue v
output (ExAny v) = formatValue v
output (ExFree v) = formatValue v
output (ExAp l r) = output l ++ output r

isFree :: String -> Bool
isFree ('-':_) = False
isFree _ = True

arbitraryEx :: ParserDesc -> Gen Example
arbitraryEx (DescMatch s) = pure $ ExMatch s s
arbitraryEx (DescMAF s (DescFollower ns)) =
  ExMAF s <$> sequenceA [arbitraryValue vt | DescNext vt _ <- ns]
arbitraryEx (DescMatchShort c) = pure $ ExMatchShort c [c]
arbitraryEx (DescFlag WithoutBundling fs _) = ExMatch <$> elements fs <*> pure ""
arbitraryEx (DescFlag WithBundling fs _) = f <$> elements fs where
  f ('-':c:[]) | c /=  '-' = ExMatchShort c ""
  f s = ExMatch s ""
arbitraryEx (DescParam vt fs _ _) = do
  form <- elements fs
  case form of
    ('-':c:[]) | c /= '-' -> oneof
      [ ExMAF form <$> sequenceA [arbitraryValue vt]
      , ExParamShort form
        <$> (arbitraryValue vt `suchThat` (not . P.null . formatValue))
      ]
    ('-':'-':_:_) -> oneof
      [ ExMAF form <$> sequenceA [arbitraryValue vt]
      , ExParamLong form <$> arbitraryValue vt
      ]
    _ -> error "illegal option form in arbitraryEx"
arbitraryEx (DescMultiParam fs (DescFollower ns) _) = do
  form <- elements fs
  vs <- sequenceA [arbitraryValue vt | DescNext vt _ <- ns]
  return $ ExMAF form vs
arbitraryEx (DescAnyArg vt _ _) = ExAny <$> arbitraryValue vt
arbitraryEx (DescFreeArg vt _ _) =
  ExFree <$> (arbitraryValue vt `suchThat` (isFree . formatValue))
arbitraryEx (DescAp l r) =
  ExAp <$> arbitraryEx l <*> arbitraryEx r

-- TODO: use set union instead of cartesian products in the implementation
-- below.
shrinkEx :: Example -> [Example]
shrinkEx (ExMatch _ _) = []
shrinkEx (ExMatchShort _ _) = []
shrinkEx (ExMAF s vs) = [ExMAF s vs' | vs' <- traverse genericShrink vs]
shrinkEx (ExParamShort s v) =
  [ExParamShort s v' | v' <- genericShrink v, not . P.null $ formatValue v']
shrinkEx (ExParamLong s v) = [ExParamLong s v' | v' <- genericShrink v]
shrinkEx (ExAny v) = [ExAny v' | v' <- genericShrink v]
shrinkEx (ExFree v) =
  [ExFree v' | v' <- genericShrink v, isFree $ formatValue v']
shrinkEx (ExAp l r) = [ExAp l' r' | l' <- shrinkEx l, r' <- shrinkEx r]

forAllEx :: Testable t => ParserDesc -> (Example -> t) -> Property
forAllEx parserDesc = forAllShrink (arbitraryEx parserDesc) shrinkEx

-- | Convenience wrapper around 'forAllEx'.
forAllExamples :: Testable t
               => ParserDesc
               -> ([String] -> String -> t)
               -> Property
forAllExamples desc f =
  forAllEx desc $ \ex ->
  forAll (glue $ pieces ex) $ \i ->
  f i (output ex)

shrinkElements :: (a -> [a]) -> [a] -> [[a]]
shrinkElements f as = work [] as where
  work ls [] = []
  work ls (r:rs) = [reverse ls ++ (r':rs) | r' <- f r] ++ work (r:ls) rs

forAllExs :: Testable t => [ParserDesc] -> ([Example] -> t) -> Property
forAllExs parserDescs = forAllShrink gen shrinkf where
  gen = traverse arbitraryEx parserDescs
  shrinkf = shrinkElements shrinkEx


{-# LANGUAGE DeriveGeneric #-}
module Options.OptStream.Test.Helpers where

import Data.Either
import Data.List hiding (union, intersect, null)
import Data.Maybe
import GHC.Generics
import Prelude hiding (null)
import Test.QuickCheck
import qualified Prelude as P

import Options.OptStream


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
isLeft' x = counterexample (kind x ++ showsPrec 11 x "") (isLeft x)
  where
    kind (Left _) = "isLeft "
    kind (Right _) = "isRight "

isRight' :: (Show a, Show b) => Either a b -> Property
isRight' x = counterexample (kind x ++ showsPrec 11 x "") (isRight x)
  where
    kind (Left _) = "isLeft "
    kind (Right _) = "isRight "



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
  s <- arbitrary `suchThat` (not . P.null)
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

instance Arbitrary Forms where
  arbitrary = arbitraryForms arbitraryLegal
  shrink = shrinkForms shrinkLegal


-- | Represents a list of legal option forms with one of them selected. It is
-- guaranteed that the selected one is always a short form.
newtype FormsS = ChosenShort Forms
  deriving Show

instance Arbitrary FormsS where
  arbitrary = ChosenShort <$> arbitraryForms arbitraryShort

  shrink (ChosenShort fs) =
    [ChosenShort fs' | fs' <- shrinkForms shrinkShort fs]


-- | Represents a list of legal option forms with one of them selected. It is
-- guaranteed that the selected one is always a long form.
newtype FormsL = ChosenLong Forms
  deriving Show

instance Arbitrary FormsL where
  arbitrary = ChosenLong <$> arbitraryForms arbitraryLong
  shrink (ChosenLong fs) = [ChosenLong fs' | fs' <- shrinkForms shrinkLong fs]


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

-- | Makes an arbitrary Follower for testing (using only <*>).
mkFollower :: [(ValueType, Metavar)] -> Follower [String]
mkFollower = traverse (uncurry mkNext)



-- * Producing command line examples for testing


-- | Arbitrary string interesting for command line testing.
arbitraryArg :: Gen String
arbitraryArg = oneof
  [ arbitrary
  , ("-" ++) <$> arbitrary
  , ("--" ++) <$> arbitrary
  ]

-- | Arbitrary character interesting for command line testing.
arbitraryChar :: Gen Char
arbitraryChar = oneof [arbitrary, pure '-']


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

instance Arbitrary Value where
  arbitrary = oneof
    [ ValueString <$> arbitraryArg
    , ValueReadInt <$> arbitrary
    , ValueChar <$> arbitraryChar
    ]

  shrink x@(ValueString _) = genericShrink x
  shrink x = ValueString (formatValue x):genericShrink x


-- | Like 'Value' but with a restriction that the value's string representation
-- is not empty.
newtype NonEmptyValue = NonEmptyValue Value
  deriving Show

instance Arbitrary NonEmptyValue where
  arbitrary = NonEmptyValue
    <$> (arbitrary `suchThat` (not . P.null . formatValue))

  shrink (NonEmptyValue x) =
    [ NonEmptyValue x'
    | x' <- shrink x
    , not . P.null $ formatValue x'
    ]


-- | Like 'Value' but with a restriction that the value's string representation
-- doesn't start with @-@.
newtype FreeValue = FreeValue { unFreeValue :: Value }
  deriving Show

instance Arbitrary FreeValue where
  arbitrary = FreeValue <$> (arbitrary `suchThat` (isFree . formatValue))

  shrink (FreeValue x) =
    [ FreeValue x'
    | x' <- shrink x
    , isFree $ formatValue x'
    ]

isFree :: String -> Bool
isFree ('-':_) = False
isFree _ = True


-- ** Full parse examples


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
    -- ^ A set containing (at least) all the strings the parser is supposed to
    -- be willing to consume. The parser should skip any string not belonging
    -- to this set.
  }


-- | Represents an example where a specific @param*@ parser should match a
-- specific block of arguments.
data ParamExample
  = ParamExample HelpChoice Forms Metavar Value
  | ParamShortExample HelpChoice FormsS Metavar NonEmptyValue
  | ParamLongExample HelpChoice FormsL Metavar Value
  deriving (Show, Generic)

instance Arbitrary ParamExample where
  arbitrary = oneof
    [ ParamExample <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ParamShortExample <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , ParamLongExample <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

  shrink e@(ParamShortExample help (ChosenShort fs) mv (NonEmptyValue val)) =
    ParamExample help fs mv val:genericShrink e
  shrink e@(ParamLongExample help (ChosenLong fs) mv val) =
    ParamExample help fs mv val:genericShrink e
  shrink e = genericShrink e

buildParamExample :: ParamExample -> Example String
buildParamExample (ParamExample help fs metavar val)
  = Example
  { parser = mkParam help (valueType val) forms metavar
  , inputs = [chosenForm fs, x]
  , result = x
  , consumes = mconcat $ map withPrefix forms
  }
  where
    forms = allForms fs
    x = formatValue val

buildParamExample
  (ParamShortExample help (ChosenShort fs) metavar (NonEmptyValue val))
  = (buildParamExample $ ParamExample help fs metavar val)
  { inputs = [chosenForm fs ++ formatValue val] }

buildParamExample (ParamLongExample help (ChosenLong fs) metavar val)
  = (buildParamExample $ ParamExample help fs metavar val)
  { inputs = [chosenForm fs ++ "=" ++ formatValue val] }


-- | Represents an example where a @freeArg*@ parser should match a specific
-- command line argument.
data FreeArgExample = FreeArgExample HelpChoice Metavar FreeValue
  deriving (Show, Generic)

instance Arbitrary FreeArgExample where
  arbitrary = FreeArgExample <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

buildFreeArgExample :: FreeArgExample -> Example String
buildFreeArgExample (FreeArgExample help metavar (FreeValue val))
  = Example
  { parser = mkFreeArg help (valueType val) metavar
  , inputs = [x]
  , result = x
  , consumes = freeArgs
  }
  where
    x = formatValue val


-- | Represents an example where a @multiParam*@ parser should match a specific
-- block of command line arguments.
data MultiParamExample = MultiParamExample HelpChoice Forms [(Metavar, Value)]
  deriving (Show, Generic)

instance Arbitrary MultiParamExample where
  arbitrary = MultiParamExample <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

buildMultiParamExample :: MultiParamExample -> Example [String]
buildMultiParamExample (MultiParamExample helpChoice fs pairs)
  = Example
  { parser = mkMultiParam helpChoice (allForms fs) $ traverse toNext pairs
  , inputs = chosenForm fs:xs
  , result = xs
  , consumes = mconcat . map singleton $ allForms fs
  }
  where
    toNext (metavar, val) = mkNext (valueType val) metavar
    xs = map (formatValue . snd) pairs

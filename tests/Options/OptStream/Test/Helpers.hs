{-# LANGUAGE DeriveGeneric #-}
module Options.OptStream.Test.Helpers where

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



-- * Producing atomic parsers for testing


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

-- | Represents an arbitrary character other than '-'.
newtype NotDash = NotDash { unNotDash :: Char }
  deriving Show

instance Arbitrary NotDash where
  arbitrary = NotDash <$> arbitrary `suchThat` (/= '-')
  shrink (NotDash c) = [NotDash c' | c' <- shrink c, c' /= '-']


-- | Represents an arbitrary legal option form.
newtype Legal = Legal { unLegal :: OptionForm }
  deriving Show

instance Arbitrary Legal where
  arbitrary = Legal <$> arbitraryLegal
  shrink (Legal s) = [Legal s' | s' <- shrinkLegal s]

-- | Represents an arbitrary list of legal option forms (possibly empty).
newtype Legals = Legals { unLegals :: [OptionForm] }
  deriving Show

instance Arbitrary Legals where
  arbitrary = Legals . map unLegal <$> arbitrary
  shrink (Legals ss) = map Legals $ shrinkList shrinkLegal ss


-- TODO: hide Legal and Legals from the constructor below to make Show output
-- more readable.


-- | Represents a set of legal option forms with one of them selected.
data Forms = Forms [OptionForm] OptionForm [OptionForm]
  deriving (Show, Generic)

allForms :: Forms -> [OptionForm]
allForms (Forms as b cs) = as ++ [b] ++ cs

chosenForm :: Forms -> OptionForm
chosenForm (Forms _ x _) = x

instance Arbitrary Forms where
  arbitrary = Forms
    <$> listOf arbitraryLegal
    <*> arbitraryLegal
    <*> listOf arbitraryLegal
  shrink (Forms as b cs) =
    [Forms as' b cs | as' <- shrinkList shrinkLegal as]
    ++ [Forms as b' cs | b' <- shrinkLegal b]
    ++ [Forms as b cs' | cs' <- shrinkList shrinkLegal cs]


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


-- TODO: unite mkFlag and mkFlagSep.

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

-- TODO: introduce mkFollower to simplify usages of mkMultiParam


-- * Producing command line examples for testing


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
    [ ValueString <$> arbitrary
    , ValueReadInt <$> arbitrary
    , ValueChar <$> arbitrary
    ]

  shrink x@(ValueString _) = genericShrink x
  shrink x = ValueString (formatValue x):genericShrink x


-- | Like 'Value' but with a restriction that the value's string representation
-- is not empty.
newtype NonEmptyValue = NonEmptyValue { unNonEmptyValue :: Value }
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

  -- TODO: introduce a helper for such shrinks.
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
  | ParamShortExample HelpChoice Legals NotDash Legals Metavar NonEmptyValue
  | ParamLongExample HelpChoice Legals (NonEmptyList Char) Legals Metavar Value
  deriving (Show, Generic)

-- TODO: introduce helpers like Forms, but separate for short and long selected
-- form.

instance Arbitrary ParamExample where
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

  shrink = genericShrink  -- TODO shrink to ParamExample too.

buildParamExample :: ParamExample -> Example String
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

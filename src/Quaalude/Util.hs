{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quaalude.Util where

import Control.Arrow (Arrow ((***)))
import Control.Lens ((^.))
import Control.Monad (filterM)
import Control.Monad.Memo
import Control.Monad.Random.Class
import Data.Biapplicative
import Data.Bitraversable
import Data.Default
import Data.Fin (Fin)
import Data.Foldable qualified as F
import Data.HList hiding ((.<.))
import Data.List.Extra (groupOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Sum (Sum, getSum))
import Data.Ord (Down (Down))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Tuple.Extra (swap)
import Data.Tuple.HT (uncurry3)
import Data.Tuple.Solo
import Data.Type.Nat qualified as N
import Data.Variant
import Data.Variant.Types
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Linear.V3 (R1 (_x), R2 (_y), R3 (_z), V3 (..))
import Overhang qualified as OH
import Quaalude.Alias
import Quaalude.Bits (bitsToInt)
import Quaalude.Collection
import Quaalude.Tracers
import Quaalude.Tuple
import Quaalude.Unary
import Relude.Unsafe qualified as U
import System.IO.Unsafe (unsafePerformIO)
import System.Random (RandomGen, getStdGen, newStdGen, randomR, setStdGen)
import System.Random.Shuffle qualified as Shuffle
import Text.Megaparsec (Parsec, Stream, parseMaybe)
import Text.Megaparsec.Char (digitChar)
import Text.ParserCombinators.Parsec
  ( ParseError,
    Parser,
    anyChar,
    between,
    char,
    choice,
    count,
    digit,
    eof,
    lookAhead,
    many1,
    manyTill,
    noneOf,
    oneOf,
    optionMaybe,
    parse,
    sepBy,
    sepBy1,
    string,
    try,
  )
import Text.RawString.QQ
import Prelude hiding (drop, filter, take)

-- Input parsing

inputPath :: Int -> FilePath
inputPath day = "input/" <> show day <> ".txt"

exampleInputPath :: Int -> FilePath
exampleInputPath day = "input/examples/" <> show day <> ".txt"

exampleInputNPath :: Int -> Int -> FilePath
exampleInputNPath day n = "input/examples/" <> show day <> "_" <> show n <> ".txt"

-- Parse input lines with the given Reader.
-- If any error occurs, fail.
readAsIO :: TR.Reader a -> FilePath -> IO [a]
readAsIO r path = readAs r . decodeUtf8 @Text <$> readFileBS path

(!<<) :: IO a -> a
(!<<) = unsafePerformIO

flines :: Text -> [Text]
flines = filter (not . T.null) . T.lines

readAs :: TR.Reader a -> Text -> [a]
readAs r text = do
  let xs = fmap r . lines $ text
  let (ls, rs) = partitionEithers xs
  case ls of
    [] -> fst <$> rs
    es -> error (show es)

readOne :: TR.Reader a -> Text -> a
readOne r text =
  case r text of
    Left e -> error (show e)
    Right (a, _) -> a

-- Quaalude utility for running a parser on a Text path
parseWithIO :: Parser a -> FilePath -> IO a
parseWithIO parser path = parseWith parser . decodeUtf8 @String <$> readFileBS (toString path)

parseWith :: Parser a -> String -> a
parseWith parser body =
  case parseEitherWith parser body of
    Right x -> x
    Left e -> error (show e)

parseEitherWith :: Parser a -> String -> Either ParseError a
parseEitherWith parser = parse parser "[input]"

parseLinesWith :: Parser a -> String -> [a]
parseLinesWith line = parseWith $ many1 (line <* eol) <* eof

parseLinesWith' :: Parser a -> String -> [a]
parseLinesWith' line = fmap (parseWith (line <* eof) . T.unpack) . lines . T.pack

parseLinesEitherWith :: Parser a -> String -> [Either ParseError a]
parseLinesEitherWith line = fmap (parse (line <* eof) "[input]" . T.unpack) . lines . T.pack

(|-?) :: String -> Parser a -> Either ParseError a
(|-?) = flip parseEitherWith

infixl 5 |-?

(|-..?) :: String -> Parser a -> [Either ParseError a]
(|-..?) = flip parseLinesEitherWith

infixl 5 |-..?

(|-..?!) :: String -> Parser a -> [a]
a |-..?! p = rights $ parseLinesEitherWith p a

infixl 5 |-..?!

(|-<..?!>) :: (Monoid a) => String -> Parser a -> a
a |-<..?!> p = mconcat (a |-..?! p)

infixl 5 |-<..?!>

(-|) :: Parser a -> String -> a
(-|) = parseWith

infixl 5 -|

(|-) :: String -> Parser a -> a
(|-) = flip parseWith

infixl 5 |-

(|-..) :: String -> Parser a -> [a]
(|-..) = flip parseLinesWith'

infixl 5 |-..

(|-.) :: String -> Parser a -> a
a |-. p = parseWith (p <* (optionMaybe eol >> eof)) a

infixl 5 |-.

(|-<..>) :: (UnMonoid m a, m ~ m' a) => String -> Parser m -> a
s |-<..> p = unMonoid . mconcat $ s |-.. (try p <|> (manyTill anyChar eof $> mempty))

infixl 5 |-<..>

(|-<.>) :: forall m a. (UnMonoid m a) => String -> Parser m -> a
s |-<.> p = unMonoid @m @a $ s |- (try p <|> (manyTill anyChar eof $> mempty))

infixl 5 |-<.>

(|-<>) :: (Monoid a) => String -> Parser [a] -> a
s |-<> p = mconcat $ s |- p

(|-..<>) :: (Monoid a) => String -> Parser a -> a
s |-..<> p = mconcat $ s |-.. p

infixl 5 |-<>

(⊢) :: String -> Parser a -> a
(⊢) = (|-)

infixl 5 ⊢

twoOf :: Parser a -> Parser (a, a)
twoOf p = (,) <$> p <*> p

spaceTab :: Parser Char
spaceTab = oneOf " \t"

spaceTabs :: Parser String
spaceTabs = many spaceTab

surrounding :: Parser a -> Parser b -> Parser b
surrounding s p = s *> p <* s

wordOf :: Parser a -> Parser a
wordOf p = spaceTabs `surrounding` p

wordsOf :: Parser a -> Parser [a]
wordsOf p = many (wordOf p)

nums :: (Read a, Num a) => Parser [a]
nums = numbers

numbers :: (Read a, Num a) => Parser [a]
numbers = many (try (optionMaybe nonnumber) *> try number <* try (optionMaybe nonnumber))

nondigit :: Parser Char
nondigit = noneOf "-0123456789."

nonnumber :: Parser String
nonnumber = many1 nondigit

-- Discard until a is parsed, does not consume beyond
oneInLineDiscarding :: Parser a -> Parser a
oneInLineDiscarding p = do
  aM <- optionMaybe (try p)
  cM <- lookAhead (try (optionMaybe (noneOf "\n")))
  case (aM, cM) of
    (Nothing, Nothing) -> fail "oneInLineDiscarding: no parse, no continuation"
    (Nothing, Just _) -> anyChar >> oneInLineDiscarding p
    (Just a, _) -> return a

-- Discard until a is parsed, does not consume beyond
oneDiscarding :: Parser a -> Parser a
oneDiscarding p = do
  aM <- optionMaybe (try p)
  cM <- lookAhead (try (optionMaybe anyChar))
  case (aM, cM) of
    (Nothing, Nothing) -> fail "oneDiscarding: no parse, no continuation"
    (Nothing, Just _) -> anyChar >> oneDiscarding p
    (Just a, _) -> return a

-- Take all a in the line, do not consume the eol
manyInLineDiscarding :: Parser a -> Parser [a]
manyInLineDiscarding p =
  let go = do
        aM <- optionMaybe (try p)
        cM <- lookAhead (try (optionMaybe (noneOf "\n")))
        case (aM, cM) of
          (Nothing, Nothing) -> return []
          (Nothing, Just _) -> (try (noneOf "\n" >> go)) <|> pure []
          (Just a, Nothing) -> return [a]
          (Just a, Just _) -> (a :) <$> ((try (noneOf "\n" >> go)) <|> pure [])
   in do
        xsM <- optionMaybe (try go)
        case xsM of
          Nothing -> fail "manyInLineDiscarding: no parse"
          Just [] -> fail "manyInLineDiscarding: no items"
          Just xs -> pure xs

data PairSep (sep :: Symbol) a b = PairSep a b

data TupSep (sep :: Symbol) t = TupSep t

type family t ⯻ (sep :: Symbol) where
  (a, b) ⯻ sep = TupSep sep (a, b)
  (a, b, c) ⯻ sep = TupSep sep (a, b, c)
  (a, b, c, d) ⯻ sep = TupSep sep (a, b, c, d)
  (a, b, c, d, e) ⯻ sep = TupSep sep (a, b, c, d, e)
  (a, b, c, d, e, f) ⯻ sep = TupSep sep (a, b, c, d, e, f)
  [a] ⯻ sep = SepByMany sep a

data LinesSep (sep :: Symbol) a = LinesSep a

data MapSep (sep :: Symbol) k v = MapSep k v

data CSV a (n :: Nat) = CSV (a :^ n)

type a ⹉ n = CSV a n

data CSVAny a = CSVAny [a]

data HListSep sep xs = HListSep (HList xs)

data SepByMany (sep :: Symbol) a = SepByMany a

data Between (l :: Symbol) (r :: Symbol) a = Between a

data ℕw = ℕw String ℕ deriving (Eq, Ord, Show)

unℕw :: ℕw -> String
unℕw (ℕw s _) = s

data ℕc = ℕc (Maybe Char) deriving (Eq, Ord, Show)

unℕc :: ℕc -> Maybe Char
unℕc (ℕc c) = c

data Exactly (sym :: Symbol) = Exactly deriving (Show, Eq, Ord)

data WithoutChars (sym :: Symbol) t = Without t deriving (Show, Eq, Ord)

type t ⊟ sym = WithoutChars sym t

type AaZz = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :: Symbol

type Whitespace = " \t\n\r" :: Symbol

type EOLs = "\n\r" :: Symbol

data OfChars (sym :: Symbol) t = OfChars t deriving (Show, Eq, Ord)

type t ⭀ sym = OfChars sym t

type family CanonicalParseListF (a :: [*]) where
  CanonicalParseListF '[] = '[]
  CanonicalParseListF (t ': ts) = CanonicalParseF t ': CanonicalParseListF ts

type family CanonicalParseF a where
  CanonicalParseF (SepByMany _ a) = CanonicalParseF [a]
  CanonicalParseF (Between _ _ a) = CanonicalParseF a
  CanonicalParseF (Exactly _) = String
  CanonicalParseF (WithoutChars _ a) = a
  CanonicalParseF (OfChars _ a) = a
  CanonicalParseF (PairSep _ a b) = CanonicalParseF (a, b)
  CanonicalParseF (TupSep _ t) = CanonicalParseF t
  CanonicalParseF (LinesSep _ a) = CanonicalParseF [a]
  CanonicalParseF (MapSep _ k v) = CanonicalParseF (Map k v)
  CanonicalParseF (CSVAny a) = [CanonicalParseF a]
  CanonicalParseF (CSV a n) = CanonicalParseF (a :^ n)
  CanonicalParseF (RangeOf a) = RangeOf a
  CanonicalParseF Char = Char
  CanonicalParseF String = String
  CanonicalParseF Text = Text
  CanonicalParseF [a] = [CanonicalParseF a]
  CanonicalParseF (Map k v) = Map (CanonicalParseF k) (CanonicalParseF v)
  CanonicalParseF (HList ts) = HList (CanonicalParseListF ts)
  CanonicalParseF (HListSep _ ts) = CanonicalParseF (HList ts)
  CanonicalParseF (Solo a) = (Solo (CanonicalParseF a))
  CanonicalParseF (a, b) = (CanonicalParseF a, CanonicalParseF b)
  CanonicalParseF (a, b, c) = (CanonicalParseF a, CanonicalParseF b, CanonicalParseF c)
  CanonicalParseF (f a b) = f (CanonicalParseF a) (CanonicalParseF b)
  CanonicalParseF (f a) = f (CanonicalParseF a)
  CanonicalParseF a = a

class CanonicalParse a where
  parseCanonical :: Parser (CanonicalParseF a)

instance CanonicalParse () where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pure ()

instance CanonicalParse Integer where
  parseCanonical =
    traceCanonical "parseCanonical" $
      number

instance CanonicalParse Natural where
  parseCanonical =
    traceCanonical "parseCanonical" $
      natural

instance CanonicalParse ℕw where
  parseCanonical =
    traceCanonical "parseCanonical" $
      do
        n <- many1 (oneOf "0123456789")
        pure $ ℕw n (U.read n)

instance CanonicalParse ℕc where
  parseCanonical =
    traceCanonical "parseCanonical" $
      do
        c <- (Just <$> (try (oneOf "0123456789"))) <|> pure Nothing
        pure $ ℕc c

instance CanonicalParse (Integer -> Integer -> Integer) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      mathOp

instance CanonicalParse (Natural -> Natural -> Natural) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      mathOp

instance (Read a) => CanonicalParse (RangeOf a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      natRange Incl

instance (CanonicalParse a, CanonicalParseF [a] ~ [CanonicalParseF a], KnownSymbol sep) => CanonicalParse (SepByMany sep a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      do
        let seps = many1 (oneOf (symbolVal (Proxy @sep)))
        many1 ((try (optionMaybe seps) *> try (parseCanonical @a) <* try (optionMaybe seps)))

instance (CanonicalParse a, KnownSymbol l, KnownSymbol r) => CanonicalParse (Between l r a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      between (parseCanonical @(Exactly l)) (parseCanonical @(Exactly r)) (try (parseCanonical @a))

instance (CanonicalParse a, CanonicalParse b, KnownSymbol sep) => CanonicalParse (PairSep sep a b) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      do
        a <- parseCanonical @a <* parseCanonical @(Exactly sep)
        b <- parseCanonical @b
        return (a, b)

instance CanonicalParse (HListSep sep '[]) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pure HNil

instance
  (CanonicalParse t) =>
  CanonicalParse (HListSep sep '[t])
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (.*.)
        <$> parseCanonical @t
        <*> pure HNil

instance
  ( CanonicalParse (HListSep sep (u ': ts)),
    CanonicalParse t,
    KnownSymbol sep
  ) =>
  CanonicalParse (HListSep sep (t ': u ': ts))
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (.*.)
        <$> parseCanonical @t
        <*> (try (optionMaybe (string (symbolVal (Proxy @sep)))) *> parseCanonical @(HListSep sep (u ': ts)))

instance
  ( Tup2ListF t ~ l,
    l ~ (head ': tail),
    pl ~ CanonicalParseListF l,
    List2Tup pl,
    List2TupF pl ~ CanonicalParseF t,
    CanonicalParse (HListSep sep l),
    CanonicalParseF (HListSep sep l) ~ HList pl,
    KnownSymbol sep
  ) =>
  CanonicalParse (TupSep sep t)
  where
  parseCanonical =
    traceCanonical ("TupSep " <> symbolVal (Proxy @sep) <> " t") $
      list2Tup <$> parseCanonical @(HListSep sep l)

instance
  (CanonicalParse (TupSep "," (a :^ n))) =>
  CanonicalParse (CSV a n)
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      parseCanonical @(TupSep "," (a :^ n))

instance (CanonicalParse a) => CanonicalParse (CSVAny a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (try (parseCanonical @a)) `sepBy` char ','

instance (KnownSymbol sep, CanonicalParse a, CanonicalParseF [a] ~ [CanonicalParseF a]) => CanonicalParse (LinesSep sep a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      ((try (parseCanonical @a)) <* many (optionMaybe eol))
        `sepBy1` (try (parseCanonical @(Exactly sep)))

instance (CanonicalParse a) => CanonicalParse (NonEmpty a) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      try do
        xs <- try $ many1 (try (parseCanonical @a <* eol))
        case nonEmpty xs of
          Nothing -> fail "Expected non-empty list"
          Just ne -> return ne

-- instance
--   {-# OVERLAPS #-}
--   ( CanonicalParse a,
--     CanonicalParse b,
--     CanonicalParseF (p a b) ~ p (CanonicalParseF a) (CanonicalParseF b),
--     Biapplicative p
--   ) =>
--   CanonicalParse (p a b)
--   where
--   parseCanonical = bipure @p <$> parseCanonical @a <*> parseCanonical @b

instance {-# OVERLAPS #-} (CanonicalParse a, CanonicalParseF [a] ~ [CanonicalParseF a]) => CanonicalParse [a] where
  parseCanonical =
    traceCanonical "parseCanonical" $
      parseCanonical @(SepByMany " " a)

instance {-# OVERLAPS #-} (CanonicalParse [a]) => CanonicalParse [[a]] where
  parseCanonical =
    traceCanonical "parseCanonical" $
      many1 (parseCanonical @[a])

instance (KnownSymbol sym) => CanonicalParse (Exactly sym) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      string (symbolVal (Proxy @sym))

instance CanonicalParse Char where
  parseCanonical =
    traceCanonical "parseCanonical" $
      noneOf "\n"

instance (KnownSymbol sym) => CanonicalParse (𝕊 ⊟ sym) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      many1 (noneOf ("\n" <> symbolVal (Proxy @sym)))

instance (KnownSymbol sym) => CanonicalParse (𝕊 ⭀ sym) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      many1 (oneOf (symbolVal (Proxy @sym)))

instance CanonicalParse 𝕋 where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pack <$> parseCanonical @𝕊

instance (CanonicalParse (𝕊 ⊟ sym)) => CanonicalParse (𝕋 ⊟ sym) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pack <$> parseCanonical @(𝕊 ⊟ sym)

instance (KnownSymbol sym) => CanonicalParse (𝕋 ⭀ sym) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pack <$> parseCanonical @(𝕊 ⭀ sym)

instance (CanonicalParse k, CanonicalParse v, Ord (CanonicalParseF k)) => CanonicalParse (Map k v) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      mkMap . un <$> parseCanonical @(NonEmpty (k, v))

instance (CanonicalParse k, CanonicalParse v, Ord (CanonicalParseF k), KnownSymbol sep) => CanonicalParse (MapSep sep k v) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      do
        kvs <- parseCanonical @(NonEmpty (TupSep sep (k, v)))
        return $ mkMap (un kvs)

class (CanonicalParse a, a ~ CanonicalParseF a) => CanonicalParseSelf a where
  parseCanonicalSelf :: Parser a
  parseCanonicalSelf =
    traceCanonical "parseCanonical" $
      parseCanonical @a

instance (CanonicalParse a, a ~ CanonicalParseF a) => CanonicalParseSelf a

class
  ( CanonicalParseSelf (HList (CanonicalTupleL t)),
    List2Tup (CanonicalTupleL t),
    List2TupF (CanonicalTupleL t) ~ CanonicalParseF t
  ) =>
  CanonicalParseTup t
  where
  parseCanonicalTuple :: Parser (CanonicalParseF t)
  parseCanonicalTuple =
    traceCanonical "parseCanonical" $
      list2Tup @(CanonicalTupleL t)
        <$> parseCanonicalSelf @(HList (CanonicalTupleL t))

instance
  ( CanonicalParseSelf (HList (CanonicalTupleL t)),
    List2Tup (CanonicalTupleL t),
    List2TupF (CanonicalTupleL t) ~ CanonicalParseF t
  ) =>
  CanonicalParseTup t

type CanonicalTupleL t = Tup2ListF (MapCanonicalParseF t)

type CanonicalTupleHList t = HList (CanonicalTupleL t)

type family MapCanonicalParseF t where
  MapCanonicalParseF () = ()
  MapCanonicalParseF (Solo a) = Solo (CanonicalParseF a)
  MapCanonicalParseF (a, b) = (CanonicalParseF a, CanonicalParseF b)
  MapCanonicalParseF (a, b, c) = (CanonicalParseF a, CanonicalParseF b, CanonicalParseF c)

-- instance {-# OVERLAPS #-} (CanonicalParseTup (a, b)) => CanonicalParse (a, b) where
--  parseCanonical = parseCanonicalTuple @(a, b)
instance
  ( CanonicalParse a,
    CanonicalParse b
  ) =>
  CanonicalParse (a, b)
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (,) <$> parseCanonical @a <*> parseCanonical @b

-- instance (CanonicalParseTup (a, b, c)) => CanonicalParse (a, b, c) where
instance
  ( CanonicalParse a,
    CanonicalParse b,
    CanonicalParse c
  ) =>
  CanonicalParse (a, b, c)
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (,,) <$> parseCanonical @a <*> parseCanonical @b <*> parseCanonical @c -- parseCanonicalTuple @(a, b, c)

instance CanonicalParse (HList '[]) where
  parseCanonical =
    traceCanonical "parseCanonical" $
      pure HNil

instance
  ( CanonicalParse t,
    CanonicalParseSelf (HList ts)
  ) =>
  CanonicalParse (HList (t ': ts))
  where
  parseCanonical =
    traceCanonical "parseCanonical" $
      (.*.) <$> parseCanonical @t <*> parseCanonicalSelf @(HList ts)

(⋯) :: (CanonicalParseSelf a) => String -> a
(⋯) = (|- parseCanonicalSelf)

traceCanonical = ptrace' False

class (CanonicalParse v) => CanonicalParseVia v a where
  parseVia :: Parser a
  default parseVia :: (CanonicalParseF v ~ a) => Parser a
  parseVia = traceCanonical "parseCanonical" $ parseCanonical @v

  (⋮) :: (CanonicalParseF v ~ a, CanonicalParse v) => String -> a
  (⋮) s = s |- parseVia @v @a

(⊏|⊐) :: forall v {a}. (CanonicalParse v, (Convable (CanonicalParseF v) a)) => String -> a
(⊏|⊐) s = (((⋮) @v s) ⊏⊐)

(⋮⊏) :: forall {f} {x} v. (CanonicalParseVia v (f x), CanonicalParseF v ~ f x, Unable f) => String -> [x]
(⋮⊏) s = let fx :: f x = ((⋮) @v @(f x) s) in (fx & (⊏) @f)

instance (CanonicalParse v, CanonicalParseF v ~ a) => CanonicalParseVia v a

(⋮×⋮) :: (CanonicalParseSelf (NonEmpty a, NonEmpty b)) => Parser (NonEmpty a, NonEmpty b)
(⋮×⋮) = parseCanonicalSelf

(⋮↵⋮) ::
  forall a b.
  (CanonicalParse (PairSep "\n" (NonEmpty a) (NonEmpty b))) =>
  Parser (CanonicalParseF (PairSep "\n" (NonEmpty a) (NonEmpty b)))
(⋮↵⋮) = parseCanonical @(PairSep "\n" (NonEmpty a) (NonEmpty b))

(⋮×⋯) :: (CanonicalParseSelf (NonEmpty a, [b])) => Parser (NonEmpty a, [b])
(⋮×⋯) = parseCanonicalSelf

(⋮↵⋯) ::
  forall a b.
  (CanonicalParse (PairSep "\n" (NonEmpty a) [b])) =>
  Parser (CanonicalParseF (PairSep "\n" (NonEmpty a) [b]))
(⋮↵⋯) = parseCanonical @(PairSep "\n" (NonEmpty a) [b])

natRange :: (Read a) => RangeSize -> Parser (RangeOf a)
natRange rs = RangeOf rs . toTuple2 <$> (natural `sepBy1` (char '-'))

naturals :: (Read a) => Parser [a]
naturals = many (try (optionMaybe nonNatural) *> try natural <* try (optionMaybe nonNatural))

mathOpChars :: String
mathOpChars = "+-*/"

mathOp :: (Integral a) => Parser (a -> a -> a)
mathOp = do
  op <- try (oneOf mathOpChars)
  case op of
    '+' -> return (+)
    '-' -> return (-)
    '*' -> return (*)
    '/' -> return div
    _ -> fail "Unknown operation"

nonMathOpChar :: Parser Char
nonMathOpChar = noneOf mathOpChars

nonMathOp :: Parser String
nonMathOp = many1 (try nonMathOpChar)

nats :: (Read a) => Parser [a]
nats = naturals

nonNatDigit :: Parser Char
nonNatDigit = noneOf "0123456789"

nonNatural :: Parser String
nonNatural = many1 (try nonNatDigit)

nat₁₀ :: Parser ℕ₁₀
nat₁₀ = do
  c <- digit
  return . fromIntegral $ digitToInt c

anyWord :: Parser String
anyWord = wordOf (many1 (noneOf " \t\n\r"))

succeed :: a -> Parser a
succeed a = lookAhead eof $> a

trying :: [Parser a] -> Parser a
trying ps = choice $ try <$> ps

parend :: Parser a -> Parser a
parend = between (char '(') (char ')')

assocsSep :: Parser kvSep -> Parser itemSep -> Parser k -> Parser v -> Parser [(k, v)]
assocsSep kvSep itemSep key value = many ((,) <$> (key <* kvSep) <*> (value <* itemSep))

mapSep :: (Ord k) => Parser kvSep -> Parser itemSep -> Parser k -> Parser v -> Parser (Map k v)
mapSep kvSep itemSep key value =
  M.fromList <$> assocsSep kvSep itemSep key value

mapSepWith :: (Ord k) => (v -> v -> v) -> Parser kvSep -> Parser itemSep -> Parser k -> Parser v -> Parser (Map k v)
mapSepWith f kvSep itemSep key value =
  M.fromListWith f <$> assocsSep kvSep itemSep key value

mapcat :: (Ord k) => String -> Parser k -> Parser v -> Parser (Map k [v])
mapcat kvSep key value = mapSepWith (<>) (string kvSep) eol key (pure <$> value)

abc :: Parser String
abc = many1 (oneOf "abcdefghijklmnopqrstuvwxyz")

abc123 :: Parser String
abc123 = many1 (oneOf "abcdefghijklmnopqrstuvwxyz0123456789")

abcABC123 :: Parser String
abcABC123 = many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")

type family CParsersF args where
  CParsersF '[] = '[]
  CParsersF (arg ': args) = Parser arg ': CParsersF args

type family CFuncArgsF argParsers where
  CFuncArgsF '[] = '[]
  CFuncArgsF (Parser arg ': argParsers) = arg ': CFuncArgsF argParsers

class CFuncArgs (argParsers :: [*]) where
  cfuncArgs :: HList argParsers -> Parser (HList (CFuncArgsF argParsers))

instance CFuncArgs '[] where
  cfuncArgs _ = return HNil

instance CFuncArgs '[Parser arg] where
  cfuncArgs (HCons argParser HNil) = do
    arg <- argParser
    return $ arg .*. HNil

instance (CFuncArgs (next ': argParsers)) => CFuncArgs (Parser arg ': next ': argParsers) where
  cfuncArgs (HCons argParser rest) = do
    arg <- argParser
    char ','
    rest <- cfuncArgs rest
    return $ arg .*. rest

data CFnName = CFnName {unCFnName :: String}

instance (KnownSymbol s) => IsLabel s CFnName where
  fromLabel = CFnName (symbolVal (Proxy @s))

cfunc ::
  ( CFuncArgsF (CParsersF args) ~ args,
    CFuncArgs (CParsersF args)
  ) =>
  CFnName ->
  HList (CParsersF args) ->
  Parser (String, HList args)
cfunc label argParsers =
  (,) <$> string (unCFnName label) <*> parend (cfuncArgs argParsers)

cfunc_ ::
  CFnName ->
  Parser String
cfunc_ label =
  string (unCFnName label) <* string "()"

cargs ::
  forall args.
  ( CFuncArgs (CParsersF args),
    CFuncArgsF (CParsersF args) ~ args
  ) =>
  CFnName ->
  HList (CParsersF args) ->
  Parser (HList args)
cargs label argParsers = snd <$> cfunc label argParsers

-- Show helpers

class TShow a where
  tshow :: (Show a) => a -> Text
  tshow = T.pack . show
  tshowM :: (Show a) => Maybe a -> Text
  tshowM Nothing = "<null>"
  tshowM (Just t) = tshow t
  ltshow :: (Show a) => a -> Text
  ltshow = T.toLower . tshow
  utshow :: (Show a) => a -> Text
  utshow = T.toUpper . tshow

instance (Show a) => TShow a

-- Typeclass helpers / functional helpers

infixl 5 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 5 <$$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infixl 5 <&&>

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip (<$$>)

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

(<:>) :: (Bifunctor f) => (a -> b) -> f a a -> f b b
(<:>) = both

infixl 5 <:>

bothM :: (Bitraversable f, Monad m) => (a -> m b) -> f a a -> m (f b b)
bothM f = bitraverse f f

same :: (Eq a) => (a, a) -> Bool
same = uncurry (==)

treverse :: (Traversable t, Monad f) => (t a -> b) -> t (f a) -> f b
treverse f = fmap f . sequence

iterateFix :: (Eq a) => (a -> a) -> a -> a
iterateFix f a
  | a == a' = a
  | otherwise = iterateFix f a'
  where
    a' = f a

stabilize :: (Eq a) => (a -> a) -> [a] -> a
stabilize f (x : xs) = go (f x) xs
  where
    go last (x : xs) = let x' = f x in if x' == last then last else go x' xs

cycleGet :: (Ord a) => Int -> [a] -> a
cycleGet n as =
  let go i (a : as) seen at
        | a ∈ seen = let (s, l) = (seen |! a, i - seen |! a) in at |! (s + (n - s) `mod` l)
        | otherwise = go (i + 1) as (seen |. (a, i)) (at |. (i, a))
   in go 0 as (mkMap []) (mkMap [])

(...) :: (Ord a) => [a] -> Int -> a
(...) = flip cycleGet

-- A symmetrical split
split :: (Arrow a) => a b c -> a (b, b) (c, c)
split f = f *** f

-- A symmetrical fanout
fanout :: (Arrow a) => a b c -> a b (c, c)
fanout f = f &&& f

duping :: ((a, a) -> b) -> a -> b
duping f = f . (\a -> (a, a))

bicomp :: (b -> c, a -> b) -> a -> c
bicomp = uncurry (.)

biap :: (a -> b, a) -> b
biap = uncurry ($)

fbiap :: (Functor f) => (a -> b, f a) -> f b
fbiap = uncurry (<$>)

fbibind :: (Monad m) => (a -> m b, m a) -> m b
fbibind = uncurry (=<<)

f &.& g = bicomp . (f &&& g)

f &$& g = biap . bimap f g

f &<$>& g = fbiap . bimap f g

(&<$>) f = fbiap . first f

infixl 0 &<$>

f &=<<& g = fbibind . bimap f g

(⇄) = flip flip

(⥢) = flip duping

infixl 4 <.>

-- Apply the given function only if the predicate holds on the input
appWhen :: (a -> Bool) -> (a -> a) -> a -> a
appWhen p f x
  | p x = f x
  | otherwise = x

-- 2-ary . 1-ary in applicative
(<.>) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
f <.> g = (.) <$> f <*> g

(<∘>) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
(<∘>) = (<.>)

infix 4 <∘>

-- 2-ary . 1-ary
(.<.) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
g .<. f = (g .) . f

infixl 4 .<.

(∘<∘) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(∘<∘) = (.<.)

infixl 4 ∘<∘

-- 1-ary . 2-ary
(.>.) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(.>.) = flip (.<.)

infixr 4 .>.

(∘>∘) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(∘>∘) = (.>.)

infixr 4 ∘>∘

-- Specific currying / conversions

toTuple2 :: [a] -> (a, a)
toTuple2 [a, b] = (a, b)
toTuple2 xs = error $ show (length xs) <> "elements in toTuple2"

toTuple3 :: [a] -> (a, a, a)
toTuple3 [a, b, c] = (a, b, c)
toTuple3 xs = error $ show (length xs) <> "elements in toTuple3"

toTuple4 :: [a] -> (a, a, a, a)
toTuple4 [a, b, c, d] = (a, b, c, d)
toTuple4 xs = error $ show (length xs) <> "elements in toTuple4"

sortT2On :: (Ord b) => (a -> b) -> (a, a) -> (a, a)
sortT2On f (a, b)
  | f a <= f b = (a, b)
  | otherwise = (b, a)

sortT2OnM :: (Ord b, Monad m) => (a -> m b) -> (a, a) -> m (a, a)
sortT2OnM f (a, b) = do
  fa <- f a
  fb <- f b
  return if fa <= fb then (a, b) else (b, a)

toList2 :: (a, a) -> [a]
toList2 (a, b) = [a, b]

toList3 :: (a, a, a) -> [a]
toList3 (a, b, c) = [a, b, c]

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f = let f' (a, b, c, d) = f a b c d in f'

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

thd4 :: (a, b, c, d) -> c
thd4 (_, _, a, _) = a

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, a) = a

perms3 :: [(Int, Int, Int) -> (Int, Int, Int)]
perms3 =
  [ \(x, y, z) -> (x, y, z),
    \(x, y, z) -> (x, z, y),
    \(x, y, z) -> (y, x, z),
    \(x, y, z) -> (y, z, x),
    \(x, y, z) -> (z, x, y),
    \(x, y, z) -> (z, y, x)
  ]

permsV3 :: [V3 a -> V3 a]
permsV3 =
  [ id,
    \p -> uncurry3 V3 (p ^. _x, p ^. _z, p ^. _y),
    \p -> uncurry3 V3 (p ^. _y, p ^. _x, p ^. _z),
    \p -> uncurry3 V3 (p ^. _y, p ^. _z, p ^. _x),
    \p -> uncurry3 V3 (p ^. _z, p ^. _x, p ^. _y),
    \p -> uncurry3 V3 (p ^. _z, p ^. _y, p ^. _x)
  ]

fromV3 :: V3 a -> (a, a, a)
fromV3 p = (p ^. _x, p ^. _y, p ^. _z)

toV3 :: (a, a, a) -> V3 a
toV3 = uncurry3 V3

fsts2 :: ((a, b1), (b2, b3)) -> (a, b2)
fsts2 ((a, _), (b, _)) = (a, b)

snds2 :: ((a1, a2), (a3, b)) -> (a2, b)
snds2 ((_, a), (_, b)) = (a, b)

-- Parser Combinator Utils

eol :: Parser Char
eol = char '\n'

whitespace :: Parser String
whitespace = try . many1 $ char ' '

number :: (Read a, Num a) => Parser a
number = do
  neg <- optionMaybe (char '-')
  nM <- readMaybe <$> try (many1 (oneOf "0123456789."))
  case nM of
    Nothing -> fail "No parse in number"
    Just n -> case neg of
      Just _ -> return (negate n)
      Nothing -> return n

nat :: (Read a) => Parser a
nat = natural

natural :: (Read a) => Parser a
natural = do
  nM <- readMaybe <$> try (many1 (oneOf "0123456789"))
  case nM of
    Nothing -> fail "No parse in natural"
    Just n -> return n

bitChar :: Parser Bool
bitChar = (char '1' >> return True) <|> (char '0' >> return False)

nBitInt :: Int -> Parser Integer
nBitInt n = bitsToInt <$> Text.ParserCombinators.Parsec.count n bitChar

coord2 :: Parser (Int, Int)
coord2 = (,) <$> (number <* char ',') <*> number

csvLine :: Parser a -> Parser [a]
csvLine a = a `sepBy` char ',' <* (eol >> eof)

csv :: Parser [String]
csv = many (noneOf ",\n") `sepBy` char ','

csvOf :: Parser a -> Parser [a]
csvOf p = p `sepBy` char ','

lineOf :: Parser a -> Parser a
lineOf p = p <* eol

linesOf :: Parser a -> Parser [a]
linesOf p = many (lineOf p)

manyOf :: String -> Parser String
manyOf = many . oneOf

manyOf1 :: String -> Parser String
manyOf1 = many1 . oneOf

manyNoneOf :: String -> Parser String
manyNoneOf = many . noneOf

manyNoneOf1 :: String -> Parser String
manyNoneOf1 = many1 . noneOf

-- Map helpers

countMap :: (Ord a, Num n) => [a] -> M.Map a n
countMap xs = M.fromListWith (+) (map (,1) xs)

counts :: (Ord a, Num n) => [a] -> M.Map a n
counts = countMap

adjustWithDefault :: (Ord k) => a -> (a -> a) -> k -> M.Map k a -> M.Map k a
adjustWithDefault def f k m = case M.lookup k m of
  Nothing -> M.insert k (f def) m
  Just a -> M.insert k (f a) m

adjustMany :: (Ord k) => (a -> a) -> [k] -> M.Map k a -> M.Map k a
adjustMany f ks m = foldl' (flip (M.adjust f)) m ks

-- Set helpers

insertMany :: (Ord a) => [a] -> Set a -> Set a
insertMany as s = foldl' (flip S.insert) s as

-- List helpers

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

batch3 :: [a] -> [[a]]
batch3 xs = toList3 <$> zip3 (drop 2 xs) (drop 1 xs) xs

pair :: a -> a -> [a]
pair a b = [a, b]

about :: (Integral a) => a -> a -> [a]
about n x = [x - n .. x + n]

listAsInt :: (Integral a) => [a] -> a
listAsInt xs = sum $ uncurry (*) <$> zip (reverse xs) [10 ^ i | i <- [0 :: Integer ..]]

unlist :: [a] -> a
unlist [a] = a
unlist as = error ("Unlist called on list of length " <> show (length as))

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a : b : cs) = (a, b) : pairs (b : cs)

loopPairs :: [a] -> [(a, a)]
loopPairs xs =
  go (xs ++ [head' xs])
  where
    go (a : b : []) = [(a, b)]
    go (a : b : rest) = (a, b) : go (b : rest)

triPairs :: (Unable t) => t a -> [(a, a)]
triPairs as' = let as = un as' in [(a, b) | (i, a) <- zip [0 .. length as - 2] as, (j, b) <- zip [0 ..] as, j > i]

triples :: (Unable t) => t a -> [(a, a, a)]
triples as' =
  let as = un as'
   in [ (a, b, c)
      | (i, a) <- zip [0 .. length as - 3] as,
        (j, b) <- zip [0 .. length as - 2] as,
        j > i,
        (k, c) <- zip [0 .. length as - 1] as,
        k > j
      ]

-- Early terminating search for n items in a thing
nSameIn :: (Ord a) => Int -> [a] -> Maybe a
nSameIn n = go M.empty
  where
    go _ [] = Nothing
    go counts (a : as)
      | counts' M.! a == n = Just a
      | otherwise = go counts' as
      where
        counts' = adjustWithDefault 0 (+ 1) a counts

-- How many xs match predicate p
count :: (a -> Bool) -> [a] -> Int
count p xs = length (filter p xs)

-- Returns unique members of a list that appear more than once
duplicates :: (Ord a) => [a] -> [a]
duplicates xs = M.keys $ M.filter (> 1) (countMap xs)

enumerate :: (Enum a) => [a]
enumerate = enumFrom (toEnum 0)

enum :: (Foldable f, Mkable f (a, b)) => (Num a, Enum a) => f b -> f (a, b)
enum xs = mk (zip [0 ..] (F.toList xs))

(..#) :: (Foldable f, Mkable f (a, b)) => (Num a, Enum a) => f b -> f (a, b)
(..#) = enum

permutationMaps :: (Enum a, Ord a) => [Map a a]
permutationMaps = M.fromList . zip enumerate <$> permutations enumerate

permuteSet :: (Ord a) => Map a a -> Set a -> Set a
permuteSet = S.map . (M.!)

-- A Solution typeclass for objects that end up representing the solution in some way

class Solution a b where
  toSolution :: a -> b

unjust :: Maybe a -> a
unjust (Just a) = a
unjust Nothing = error "unjust Nothing"

-- op ? a = (((fromMaybe a) .) . flip op)
--
-- infixl 1 ?

-- Tuple helpers

-- rangeFromTo not caring about ordering
range :: (Ord a, Enum a) => a -> a -> [a]
range a b = [min a b .. max a b]

(|...|) :: (Ord a, Enum a, Mkable f a) => a -> a -> f a
a |...| b = mk [min a b .. max a b]

infix 0 |...|

(-!) :: (Num a) => a -> a -> a
(-!) = flip (-)

-- Random helpers

randomElement :: (RandomGen g) => [a] -> g -> (a, g)
randomElement xs g = let (i, g') = randomR (0, length xs - 1) g in (xs !! i, g')

randomElements :: (RandomGen g) => Int -> [a] -> g -> ([a], g)
randomElements n xs g = foldl' (\(as, g') _ -> let (a, g'') = randomElement xs g' in (a : as, g'')) ([], g) [1 .. n]

randomElementIO :: [a] -> IO a
randomElementIO xs = do
  g <- getStdGen
  let (a, g') = randomElement xs g
  setStdGen g'
  return a

randomElementsIO :: Int -> [a] -> IO [a]
randomElementsIO n xs = do
  g <- getStdGen
  let (as, g') = randomElements n xs g
  setStdGen g'
  return as

randomFromEnum :: (RandomGen g, Enum a, Bounded a) => g -> (a, g)
randomFromEnum g =
  let items = [minBound .. maxBound]
   in randomElement items g

randomFromEnumIO :: (Enum a, Bounded a) => IO a
randomFromEnumIO = do
  g <- getStdGen
  let (a, g') = randomFromEnum g
  setStdGen g'
  return a

shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle xs = Shuffle.shuffle' xs (length xs)

-- Because default random shuffling is pretty awful
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
  g <- getStdGen
  let xs' = Shuffle.shuffle' xs (length xs) g
  g' <- newStdGen
  setStdGen g'
  return xs'

-- Quoting

-- Like [r||] but remove per-line trailing and leading whitespace
txt :: QuasiQuoter
txt = r {quoteExp = (quoteExp r) . T.unpack . T.unlines . fmap T.strip . T.lines . T.strip . T.pack}

-- Conversion

class As to from where
  as :: from -> to

instance (As a ℝ) => Inf a where
  infinity = as @a $ U.read @ℝ "Infinity"

instance (As a c, As b d) => As (a, b) (c, d) where
  as (a, b) = (as a, as b)

instance (As b ℝ) => As (Σ b) ℝ where
  as = Σ . as

instance (As b n) => As (Σ b) (Σ n) where
  as (Σ n) = Σ (as n)

instance As ℤ ℝ where
  as = round

instance As ℝ ℤ where
  as = fromIntegral

instance As ℝ ℤ₆₄ where
  as = fromIntegral

instance As ℤ 𝔹 where
  as from = bool 0 1 from

instance As 𝔹 ℤ where
  as 0 = False
  as _ = True

instance As ℤ ℤ₆₄ where
  as = fromIntegral

instance As ℤ₆₄ ℤ where
  as = fromIntegral

instance (N.SNatI n) => As (Fin n) ℤ where
  as = fromIntegral

instance (N.SNatI n) => As ℤ (Fin n) where
  as = toInteger

instance As Text Char where
  as = T.singleton

-- Memo

data Mℤ k = Mℤ ℤ k deriving (Ord, Eq)

type MM k v = Memo k v v

type k .->. v = k -> MM k v

(.$.) :: (Ord k) => (k .->. v) -> k -> MM k v
(.$.) = memo

(.<$>.) :: (Ord k, Functor f) => (k .->. v) -> f k -> f (MM k v)
f .<$>. a = memo f <$> a

(.=<<.) :: (Ord k, Traversable m) => (k .->. v) -> m k -> _
f .=<<. a = sequence $ f .<$>. a

class Runnable f where
  run :: f a -> a

instance Runnable (Memo k v) where
  run = startEvalMemo

instance Convable (Memo k v a) a where
  co = run

-- Defaults

instance Default Char where
  def = ' '

pad :: Int -> c -> [c] -> [c]
pad l c x = replicate (l - length x) c ++ x

-- | Split list by groups of one or more sep.
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy isSep lst =
  let (first, rest) = break isSep lst
   in first : splitBy isSep (dropWhile isSep rest)

pcases :: a -> [(a -> Bool, b)] -> b
pcases a ((p, b) : cs)
  | p a = b
  | otherwise = pcases a cs

cases :: (Eq a) => a -> [(a, b)] -> b
cases a ((a', b) : cs)
  | a == a' = b
  | otherwise = cases a cs

a ?> cs = pcases a cs

infixr 1 ?>

a ==?> cs = cases a cs

infixr 1 ==?>

a ->> c = (a, c)

infixr 1 ->>

(⊨) :: a -> Bool
(⊨) _ = True

(⊭) :: a -> Bool
(⊭) _ = False

class SymbolList (cs :: Symbol) l where
  symbolList :: [Char]

instance SymbolList "" '[] where
  symbolList = []

instance
  ( SymbolList cs l,
    ConsSymbol c cs ~ css,
    KnownChar c
  ) =>
  SymbolList css ((SChar c) ': l)
  where
  symbolList = charVal (Proxy @c) : symbolList @cs @l

symbolHead ::
  forall css c cs.
  ( ConsSymbol c cs ~ css,
    KnownChar c
  ) =>
  Maybe Char
symbolHead = Just (charVal (Proxy @c))

type family ListToSymbol l where
  ListToSymbol '[] = ""
  ListToSymbol (c ': l) = ConsSymbol c (ListToSymbol l)

type family SymbolToList (s :: Symbol) :: [Char] where
  SymbolToList "" = '[]
  SymbolToList s = SymbolToListM (UnconsSymbol s)

type family SymbolToListM (s :: Maybe (Char, Symbol)) :: [Char] where
  SymbolToListM ('Just '(c, s)) = c ': SymbolToList s
  SymbolToListM 'Nothing = '[]

type family SChars cs where
  SChars '[] = '[]
  SChars (c ': cs) = SChar c ': SChars cs

type SymSChars s = SChars (SymbolToList s)

type CharV s = V (SymSChars s)

type 𝘊 s = CharV s

sortGroupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
sortGroupOn f xs = groupOn f $ sortOn f xs

nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOn f xs = head' <$> sortGroupOn f xs

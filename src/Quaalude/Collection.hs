{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quaalude.Collection where

import Control.Lens (element, (.~))
import Data.Array qualified as A
import Data.Biapplicative
import Data.Bimap qualified as BM
import Data.ByteString.Lazy as BL (ByteString)
import Data.ByteString.Lazy.Char8 as CL8 (pack, unpack)
import Data.Char qualified as C
import Data.Foldable qualified as F
import Data.HList
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.List qualified as L
import Data.List.Extra qualified as LE
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.Split qualified as LS
import Data.Map.Strict qualified as M
import Data.MonoTraversable
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Quaalude.Alias
import Quaalude.Tracers
import Quaalude.Unary
import Relude.Unsafe qualified as U
import Prelude hiding (drop, filter, splitAt, take)

traceMk :: String -> a -> a
traceMk s = traceIf False $ "mk: " <> s

traceUn :: String -> a -> a
traceUn s = traceIf False $ "un: " <> s

traceCo :: String -> a -> a
traceCo s = traceIf False $ "co: " <> s

class Packable a b where
  pack :: a -> b
  unpack :: b -> a

instance Packable String T.Text where
  pack = T.pack
  unpack = T.unpack

instance Packable String String where
  pack = id
  unpack = id

instance Packable Text Text where
  pack = id
  unpack = id

instance Packable String BL.ByteString where
  pack = CL8.pack
  unpack = CL8.unpack

class MkWithable f where
  mkWith :: (Ord k, Unable t) => (v -> v -> v) -> t (k, v) -> f k v

instance MkWithable Map where
  mkWith f xs = M.fromListWith f (un xs)

class Mkable f a where
  mk :: [a] -> f a

instance Mkable [] a where
  mk = id ∘ traceMk "[] a"

instance Mkable NonEmpty a where
  mk (a : as) = traceMk "NonEmpty a" $ a :| as

instance Mkable V.Vector a where
  mk = mkVec ∘ traceMk "Vector a"

instance Mkable Seq a where
  mk = mkSeq ∘ traceMk "Seq a"

instance (Ord a) => Mkable Set a where
  mk = mkSet ∘ traceMk "Set a"

pattern Seq₁ :: a -> Seq a
pattern Seq₁ a = a SQ.:<| SQ.Empty

mk₁ :: (Mkable f a) => a -> f a
mk₁ = mk . pure

class MkableKey f where
  mkKey :: (Ord k) => [(k, v)] -> f k v

instance MkableKey Map where
  mkKey = mkMap

instance MkableKey PQ.MinPQueue where
  mkKey = mkMinQ

class MkableOrd f where
  mkOrd :: (Ord a) => [a] -> f a

instance MkableOrd Set where
  mkOrd = mkSet

class Unable f where
  un :: f a -> [a]

instance Unable [] where
  un = id ∘ traceUn "[]"

instance Unable NonEmpty where
  un (a :| as) = traceUn "NonEmpty" $ a : as

instance Unable V.Vector where
  un = unVec ∘ traceUn "Vector"

instance Unable Set where
  un = unSet ∘ traceUn "Set"

instance Unable (Map k) where
  un = fmap snd ∘ unMap ∘ traceUn "Map"

instance (Ord k) => Unable (MinQ k) where
  un = F.toList ∘ traceUn "MinQ"

instance Unable Seq where
  un = unSeq ∘ traceUn "Seq"

class UnableKey f where
  unKey :: f k v -> [(k, v)]

instance UnableKey Map where
  unKey = unMap

instance UnableKey BM.Bimap where
  unKey = unBimap

coMk :: (Mkable f y, a ~ [x], Convable x y, c ~ f y) => a -> c
coMk = mk ∘ fmap co ∘ traceCo "default via id"

class Convable a c where
  co :: a -> c
  default co :: (Mkable f y, a ~ [x], Convable x y, c ~ f y) => a -> c
  co = coMk

-- instance {-# INCOHERENT #-} Convable a [a] where
--  co = pure ∘ traceCo "a [a]"

-- instance {-# INCOHERENT #-} (Unable f, Convable [a] b) => Convable (f a) b where
--   co = co . un ∘ traceCo "(f a) b"

instance {-# OVERLAPS #-} (a ~ a') => Convable a a' where
  co = id ∘ traceCo "a a"

instance {-# OVERLAPS #-} (Convable a b, Unable f) => Convable (f a) [b] where
  co = fmap co ∘ un ∘ traceCo "(f a) [b]"

-- instance {-# OVERLAPPING #-} (Bifunctor f, Convable a c, Convable b d) => Convable (f a b) (f c d) where
--   co = bimap (co @a @c) (co @b @d) ∘ traceCo "(f a b) (f c d)"
--
-- instance {-# OVERLAPPING #-} (Trifunctor p, Convable a d, Convable b e, Convable c f) => Convable (p a b c) (p d e f) where
--   co = trimap (co @a @d) (co @b @e) (co @c @f) ∘ traceCo "(p a b c) (p d e f)"

instance {-# OVERLAPPING #-} (Ord a', Convable [a] [a']) => Convable [a] (Set a') where
  co = mkSet . co @[a] @[a'] ∘ traceCo "[a] (Set a')"

instance {-# OVERLAPPING #-} (Ord k', Convable [(k, v)] [(k', v')]) => Convable [(k, v)] (Map k' v') where
  co = mkMap ∘ co ∘ traceCo "[(k, v)] (Map k' v')"

instance (Convable a b) => Convable [a] (NonEmpty b)

instance (Convable a b) => Convable [a] (Vector b)

instance
  {-# OVERLAPPING #-}
  ( Semigroup (m k),
    SwapWithable Map m k a
  ) =>
  Convable (Map k a) (Map a (m k))
  where
  co = swapcat ∘ traceCo "String Text"

instance {-# OVERLAPPING #-} Convable Text String where
  co = Quaalude.Collection.unpack ∘ traceCo "Text String"

instance {-# OVERLAPPING #-} Convable String Text where
  co = Quaalude.Collection.pack ∘ traceCo "String Text"

-- instance {-# INCOHERENT #-} (Unable f) => Convable (f a) [a] where
--  co = un ∘ traceCo "(f a) [a]"

-- instance {-# INCOHERENT #-} (UnableKey f) => Convable (f k v) [(k, v)] where
--   co = unKey ∘ traceCo "(f k v) [(k, v)]"

-- instance {-# OVERLAPPING #-} (Ord a) => Convable [a] (Set a) where
--  co = mkSet ∘ traceCo "[a] (Set a)"

-- instance {-# OVERLAPPING #-} Convable (Map k v) [(k, v)] where
-- co = unMap ∘ traceCo "(Map k v) [(k, v)]"

-- instance {-# INCOHERENT #-} (Mkable f a) => Convable [a] (f a) where
--  co = mk ∘ traceCo "[a] (f a)"

-- instance {-# INCOHERENT #-} (MkableKey f, Ord k) => Convable [(k, v)] (f k v) where
--  co = mkKey ∘ traceCo "[(k, v)] (f k v)"

-- instance {-# OVERLAPPABLE #-} (Convable a b, Convable b c) => Convable a c where
--   co a = co (traceCo "a c via b" (co a :: b))

(⊏⊐) :: (Convable a c) => a -> c
(⊏⊐) = co

infixl 1 ⊏⊐

(⊏) :: (Unable f) => f a -> [a]
(⊏) = un

infixl 1 ⊏

(⊐) :: (Mkable f a) => [a] -> f a
(⊐) = mk

infixl 1 ⊐

class SplitOnable a where
  splitOn :: a -> a -> [a]

instance (Eq a) => SplitOnable [a] where
  splitOn = LS.splitOn

instance SplitOnable Text where
  splitOn = T.splitOn

chunksOf :: Int -> [a] -> [[a]]
chunksOf = LE.chunksOf

minimum :: (Ord a) => [a] -> a
minimum = L.minimum

maximum :: (Ord a) => [a] -> a
maximum = L.maximum

nub :: (Eq a) => [a] -> [a]
nub = L.nub

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = L.foldl1

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' = L.foldl1'

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = L.foldr1

mkVec :: [a] -> V.Vector a
mkVec = V.fromList

unVec :: V.Vector a -> [a]
unVec = V.toList

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

unSet :: Set a -> [a]
unSet = S.toList

class Insertable f a where
  (<-|) :: f a -> a -> f a
  default (<-|) :: f a -> a -> f a
  (<-|) = flip (|->)
  (|->) :: a -> f a -> f a
  default (|->) :: a -> f a -> f a
  (|->) = flip (<-|)

instance Insertable [] a where
  (|->) = (:)

instance Insertable Seq a where
  (|->) = (<|)

instance (Ord a) => Insertable Set a where
  (|->) = S.insert

class Differenceable f a where
  (\\) :: f a -> f a -> f a
  (∖) :: f a -> f a -> f a
  (∖) = (\\)

instance (Eq a) => Differenceable [] a where
  (\\) = (L.\\)

instance (Ord a) => Differenceable Set a where
  (\\) = (S.\\)

instance (Eq a) => Differenceable Seq a where
  a \\ b = SQ.fromList $ F.toList a \\ F.toList b

class Sizable a where
  size :: (Integral i) => a -> i

instance Sizable Int where
  size = fromIntegral

instance Sizable Integer where
  size = fromIntegral

instance Sizable [a] where
  size = fromIntegral . L.length

instance Sizable (NonEmpty a) where
  size = fromIntegral . F.length

instance Sizable (V.Vector a) where
  size = fromIntegral . V.length

instance Sizable (Set a) where
  size = fromIntegral . S.size

instance Sizable (Map k v) where
  size = fromIntegral . M.size

instance Sizable (Seq a) where
  size = fromIntegral . SQ.length

instance Sizable (PQ.MinPQueue k v) where
  size = fromIntegral . PQ.size

instance Sizable (A.Array i e) where
  size = fromIntegral . F.length

class Memberable a b | b -> a where
  (∈) :: a -> b -> Bool
  (∉) :: a -> b -> Bool
  a ∉ b = not $ a ∈ b
  (∋) :: b -> a -> Bool
  (∋) = flip (∈)
  (∌) :: b -> a -> Bool
  (∌) = flip (∉)

instance (Ord a) => Memberable a (Set a) where
  (∈) = S.member
  (∉) = S.notMember

instance (Eq a) => Memberable a [a] where
  (∈) = L.elem
  (∉) = L.notElem

instance (Eq a) => Memberable a (V.Vector a) where
  (∈) = V.elem
  (∉) = V.notElem

instance (Ord a) => Memberable a (Map a b) where
  (∈) = M.member
  (∉) = M.notMember

instance (A.Ix i) => Memberable i (A.Array i e) where
  i ∈ a = A.inRange (A.bounds a) i

instance (Ord a) => Unionable (Set a) where
  (∪) = S.union

instance (Ord k) => Unionable (Map k v) where
  (∪) = M.union

instance Unionable [a] where
  (∪) = (L.++)

instance Unionable (V.Vector a) where
  (∪) = (V.++)

instance (Ord a) => Intersectable (Set a) where
  (∩) = S.intersection

instance (Ord k) => Intersectable (Map k v) where
  (∩) = M.intersection

instance (Eq a) => Intersectable [a] where
  (∩) = L.intersect

instance (Eq a) => Intersectable (V.Vector a) where
  a ∩ b = mkVec $ L.intersect (V.toList a) (V.toList b)

class Ixable i f where
  (!!) :: f a -> i -> a
  (!.) :: f a -> (i, a) -> f a
  (!?) :: f a -> i -> Maybe a

instance (Integral i) => Ixable i [] where
  l !! i = l L.!! fromIntegral i
  l !. (i, a) = l & element (fromIntegral i) .~ a
  l !? i = l L.!? fromIntegral i

instance (Integral i) => Ixable i NonEmpty where
  (l :| _) !! 0 = l
  (_ :| ls) !! i = ls L.!! fromIntegral (i - 1)
  (_ :| ls) !. (0, a) = (a :| ls)
  (l :| ls) !. (i, a) = l :| (ls & element (fromIntegral (i - 1)) .~ a)
  (l :| _) !? 0 = Just l
  (_ :| ls) !? i = ls L.!? fromIntegral (i - 1)

instance (Integral i) => Ixable i V.Vector where
  v !! i = v V.! fromIntegral i
  v !. (i, a) = v V.// [(fromIntegral i, a)]
  v !? i = v V.!? fromIntegral i

instance (Integral i) => Ixable i SQ.Seq where
  s !! i = let Just a = s SQ.!? fromIntegral i in a
  s !. (i, a) = s & element (fromIntegral i) .~ a
  s !? i = s SQ.!? fromIntegral i

instance (Integral i) => Ixable i IntMap where
  m !! i = m IM.! fromIntegral i
  m !. (i, a) = IM.insert (fromIntegral i) a m
  m !? k = IM.lookup (fromIntegral k) m

class Gettable f k v where
  (|!) :: f k v -> k -> v
  default (|!) :: (Ixable k (f k)) => f k v -> k -> v
  (|!) = (!!)

instance (Ord k) => Gettable Map k v where
  (|!) = (M.!)

instance (A.Ix i) => Gettable A.Array i e where
  (|!) = (A.!)

class MaybeGettable f k v where
  (|?) :: f k v -> k -> Maybe v
  default (|?) :: (Ixable k (f k)) => f k v -> k -> Maybe v
  (|?) = (!?)

instance (Ord k) => MaybeGettable Map k v where
  (|?) = flip M.lookup

instance (A.Ix i) => MaybeGettable A.Array i e where
  a |? i = if i ∈ a then Just (a |! i) else Nothing

class ValueGettable f k v | f -> v where
  (|?>) :: f -> v -> [k]
  (|!>) :: f -> v -> k
  default (|!>) :: f -> v -> k
  xs |!> v = case (xs |?> v) of
    [] -> error "ValueGettable |!>: not found"
    (k : _) -> k

instance (Eq a, Integral i) => ValueGettable [a] i a where
  l |?> a = fromIntegral <$> L.elemIndices a l

instance (Ord k, Ord v) => ValueGettable (Map k v) k v where
  m |?> v = swapcat m |? v ? []

class Settable f k v where
  (|.) :: f k v -> (k, v) -> f k v
  default (|.) :: (Ixable k (f k)) => f k v -> (k, v) -> f k v
  (|.) = (!.)

instance (Ord k) => Settable Map k v where
  m |. (k, v) = M.insert k v m

instance (A.Ix i) => Settable A.Array i e where
  a |. (i, e) = a A.// [(i, e)]

instance (Ord k) => Settable PQ.MinPQueue k v where
  m |. (k, v) = PQ.insert k v m

class Modifiable f k v where
  (|~) :: f k v -> (k, v -> v) -> f k v
  default (|~) :: (Gettable f k v, Settable f k v) => f k v -> (k, v -> v) -> f k v
  xs |~ (k, f) = xs |. (k, (f (xs |! k)))

instance (Ord k) => Modifiable Map k v where
  m |~ (k, f) = M.adjust f k m

instance (A.Ix i) => Modifiable A.Array i e where
  a |~ (i, f)
    | i ∈ a = a A.// [(i, f (a |! i))]
    | otherwise = a

class Keysable f k where
  keys :: f -> [k]

instance Keysable (Map k v) k where
  keys = M.keys

class Valuesable f k v where
  values :: f k v -> [v]

instance Valuesable Map k v where
  values = M.elems

class Itemsable f k v where
  items :: f k v -> [(k, v)]

instance Itemsable Map k v where
  items = M.toList

class Deletable m k where
  delete :: k -> m -> m
  default delete :: k -> m -> m
  delete = flip (|\)

  (|\) :: m -> k -> m
  default (|\) :: m -> k -> m
  (|\) = flip delete

  (|\..) :: (Foldable f) => m -> f k -> m
  default (|\..) :: (Foldable f) => m -> f k -> m
  xs |\.. ks = foldl' (|\) xs ks

  (∸) :: m -> k -> m
  (∸) = (|\)

instance (Ord k) => Deletable (Map k v) k where
  delete = M.delete

instance (Ord a) => Deletable (Set a) a where
  delete = S.delete

instance (Eq a) => Deletable [a] a where
  delete = L.delete

(∅) :: (Monoid a) => a
(∅) = mempty

ø :: (Monoid a) => a
ø = (∅)

class ConvMonoidLeft f g where
  (<⊕) :: f a -> g a -> f a

class ConvMonoidRight f g where
  (⊕>) :: f a -> g a -> g a

class ConvMonoid f g h where
  (<⊕>) :: f a -> g a -> h a

instance (forall a. Semigroup (f a), forall a. Convable (g a) (f a)) => ConvMonoidLeft f g where
  a <⊕ b = a <> co b

instance (forall a. Semigroup (g a), forall a. Convable (f a) (g a)) => ConvMonoidRight f g where
  a ⊕> b = co a <> b

instance (forall a. Semigroup (h a), forall a. Convable (f a) (h a), forall a. Convable (g a) (h a)) => ConvMonoid f g h where
  a <⊕> b = co a <> co b

(⊆) :: (Ord a) => Set a -> Set a -> Bool
(⊆) = S.isSubsetOf

(⊈) :: (Ord a) => Set a -> Set a -> Bool
a ⊈ b = not (a ⊆ b)

(⊂) :: (Ord a) => Set a -> Set a -> Bool
(⊂) = S.isProperSubsetOf

(⊄) :: (Ord a) => Set a -> Set a -> Bool
a ⊄ b = not (a ⊂ b)

(⊇) :: (Ord a) => Set a -> Set a -> Bool
(⊇) = flip S.isSubsetOf

(⊉) :: (Ord a) => Set a -> Set a -> Bool
a ⊉ b = not (a ⊇ b)

(⊃) :: (Ord a) => Set a -> Set a -> Bool
(⊃) = flip S.isProperSubsetOf

(⊅) :: (Ord a) => Set a -> Set a -> Bool
a ⊅ b = not (a ⊃ b)

class SetMap f where
  setMap :: (Ord b) => (a -> b) -> f a -> f b

instance SetMap Set where
  setMap = S.map

setConcat :: (Ord a) => Set (Set a) -> Set a
setConcat = S.unions . S.toList

setConcatMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
setConcatMap f s = setConcat $ setMap f s

setFilter :: (a -> Bool) -> Set a -> Set a
setFilter = S.filter

mkHashMap :: (Hashable k) => [(k, v)] -> HashMap k v
mkHashMap = HM.fromList

unHashMap :: HashMap k v -> [(k, v)]
unHashMap = HM.toList

mkHashMapWith :: (Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
mkHashMapWith = HM.fromListWith

mkMap :: (Ord k) => [(k, v)] -> Map k v
mkMap = M.fromList

unMap :: Map k v -> [(k, v)]
unMap = M.toList

mkMapWith :: (Ord k) => (v -> v -> v) -> [(k, v)] -> Map k v
mkMapWith = M.fromListWith

mapFilter :: (v -> Bool) -> Map k v -> Map k v
mapFilter = M.filter

mapSplit :: (Ord k) => k -> Map k v -> (Map k v, Map k v)
mapSplit = M.split

(|/) :: (Ord k) => Map k v -> k -> Map k v
m |/ k = M.delete k m

mkBimap :: (Ord a, Ord b) => [(a, b)] -> BM.Bimap a b
mkBimap = BM.fromList

unBimap :: BM.Bimap a b -> [(a, b)]
unBimap = BM.toList

mkSeq :: [a] -> Seq a
mkSeq = SQ.fromList

unSeq :: Seq a -> [a]
unSeq = F.toList

emptySeq :: Seq a
emptySeq = SQ.empty

(<|) :: a -> Seq a -> Seq a
(<|) = (SQ.<|)

infixr 5 <|

(⊲) :: a -> Seq a -> Seq a
(⊲) = (SQ.<|)

infixr 5 ⊲

(|>) :: Seq a -> a -> Seq a
(|>) = (SQ.|>)

infixl 5 |>

(⊳) :: Seq a -> a -> Seq a
(⊳) = (SQ.|>)

infixl 5 ⊳

(><) :: Seq a -> Seq a -> Seq a
(><) = (SQ.><)

infixr 5 ><

(⋈) :: Seq a -> Seq a -> Seq a
(⋈) = (SQ.><)

infixr 5 ⋈

(>/<) :: (Eq a) => Seq a -> a -> Seq a
s >/< a = foldl' (flip SQ.deleteAt) s (SQ.elemIndicesL a s)

infixr 5 >/<

mkMinQ :: (Ord k) => [(k, a)] -> PQ.MinPQueue k a
mkMinQ = PQ.fromList

mkQ :: (Ord k) => [(k, a)] -> PQ.MinPQueue k a
mkQ = PQ.fromList

mkQ₁ :: (Ord k) => (a -> k) -> a -> PQ.MinPQueue k a
mkQ₁ loss a = mkQ [(loss a, a)]

qInsert :: (Ord k) => (a -> k) -> a -> PQ.MinPQueue k a -> PQ.MinPQueue k a
qInsert loss a q = q |. (loss a, a)

qAppend :: (Ord k) => (a -> k) -> [a] -> PQ.MinPQueue k a -> PQ.MinPQueue k a
qAppend loss as q = foldl' (\q a -> qInsert loss a q) q as

nullQ :: PQ.MinPQueue k a -> Bool
nullQ = PQ.null

type MinQ = PQ.MinPQueue

pattern NullQ :: PQ.MinPQueue k a
pattern NullQ <- (nullQ -> True)

pattern (:<!) :: (Ord k) => (k, a) -> PQ.MinPQueue k a -> PQ.MinPQueue k a
pattern ka :<! q <- (PQ.deleteFindMin -> (ka, q))
  where
    ka :<! q = q |. ka

pattern (:<!!) :: (Ord k) => a -> PQ.MinPQueue k a -> PQ.MinPQueue k a
pattern a :<!! q <- (first snd . PQ.deleteFindMin -> (a, q))

(<!) :: (Ord k) => PQ.MinPQueue k a -> ((k, a), PQ.MinPQueue k a)
(<!) = PQ.deleteFindMin

(<!!) :: (Ord k) => PQ.MinPQueue k a -> (a, PQ.MinPQueue k a)
(<!!) = first snd . PQ.deleteFindMin

mkArray :: (A.Ix i) => (i, i) -> [(i, e)] -> A.Array i e
mkArray = A.array

uhead :: [a] -> a
uhead = \case
  [] -> error "uhead: empty list"
  (x : _) -> x

utail :: [a] -> [a]
utail = \case
  [] -> error "utail: empty list"
  (_ : xs) -> xs

uinit :: [a] -> [a]
uinit = U.init

ulast :: [a] -> a
ulast = U.last

uread :: (Read a) => String -> a
uread = U.read

digitToInt :: Char -> Int
digitToInt = C.digitToInt

intToDigit :: Int -> Char
intToDigit = C.intToDigit

(<%>) :: (Functor g, HMapOut f l e) => f -> g (HList l) -> g [e]
f <%> xs = hMapOut f <$> xs

hom f xs = f <$> (id <%> xs)

hdup a = a .*. a .*. HNil

cartesian :: [a] -> [b] -> [(a, b)]
cartesian = liftA2 (,)

class Swappable f a b where
  swap :: f a b -> f b a
  default swap :: (Ord b, UnableKey f, MkableKey f) => f a b -> f b a
  swap = mkKey . fmap Prelude.swap . unKey

instance Swappable (,) a b where
  swap = Prelude.swap

class SwapWithable f m a b where
  swapWith :: (m a -> m a -> m a) -> f a b -> f b (m a)
  default swapWith :: (Applicative m, Ord b, UnableKey f, MkWithable f) => (m a -> m a -> m a) -> f a b -> f b (m a)
  swapWith f = mkWith f . fmap (second pure . Prelude.swap) . unKey

instance (Ord k, Ord v) => Swappable Map k v

instance (Ord k, Ord v, Applicative m) => SwapWithable Map m k v

swapcat :: (SwapWithable f m a b, Semigroup (m a)) => f a b -> f b (m a)
swapcat = swapWith (<>)

class Filterable f a where
  filter :: (a -> Bool) -> f a -> f a
  default filter :: (a -> Bool) -> f a -> f a
  filter = (<-?-|)

  (|-?->) :: f a -> (a -> Bool) -> f a
  default (|-?->) :: f a -> (a -> Bool) -> f a
  (|-?->) = flip (<-?-|)

  (<-?-|) :: (a -> Bool) -> f a -> f a
  default (<-?-|) :: (a -> Bool) -> f a -> f a
  (<-?-|) = filter

instance Filterable [] a where
  filter = L.filter

instance Filterable NonEmpty a where
  filter p xs =
    case nonEmpty (NonEmpty.filter p xs) of
      Nothing -> error "NonEmpty filtered to []"
      Just ne -> ne

instance Filterable Set a where
  filter = S.filter

instance Filterable Seq a where
  filter = SQ.filter

instance Filterable (Map k) v where
  filter = M.filter

instance (Ord k) => Filterable (MinQ k) a where
  filter = PQ.filter

class Takeable n f a where
  take :: n -> f a -> f a

instance (Integral n) => Takeable n [] a where
  take n = L.take (fromIntegral n)

instance (Integral n) => Takeable n NonEmpty a where
  take n (a :| as) = a :| L.take (fromIntegral $ n - 1) as

instance (Integral n) => Takeable n V.Vector a where
  take n = V.take (fromIntegral n)

instance (Integral n) => Takeable n SQ.Seq a where
  take n = SQ.take (fromIntegral n)

instance (Ord a, Integral n) => Takeable n Set a where
  take n = mkSet ∘ take (fromIntegral n) ∘ unSet

instance (Ord k, Integral n) => Takeable n (MinQ k) v where
  take _ NullQ = PQ.empty
  take 0 _ = PQ.empty
  take n ((k, a) :<! q) = PQ.insert k a (take (n - 1) q)

class Droppable n f a where
  drop :: n -> f a -> f a

instance (Integral n) => Droppable n [] a where
  drop n = L.drop (fromIntegral n)

instance (Integral n) => Droppable n V.Vector a where
  drop n = V.drop (fromIntegral n)

instance (Integral n) => Droppable n Seq a where
  drop n = SQ.drop (fromIntegral n)

class Headable f a where
  head' :: f a -> a

instance Headable [] a where
  head' = \case
    [] -> error "head': empty list"
    (x : _) -> x

instance (Ord a) => Headable Set a where
  head' = minimum . un

class Tailable f a where
  tail' :: f a -> f a

instance Tailable [] a where
  tail' = \case
    [] -> error "tail': empty list"
    (_ : xs) -> xs

instance (Ord a) => Tailable Set a where
  tail' s = S.delete (head' s) s

data Ť f a = Ť (f a)

data Ȟ f a = Ȟ (f a)

type instance ApplyUF (Ȟ f a) = a

type instance ApplyUF (Ť f a) = f a

instance (Headable f a) => ApplyU (Ȟ f a) a where
  (!>) (Ȟ fa) = head' fa

instance (Tailable f a) => ApplyU (Ť f a) (f a) where
  (!>) (Ť fa) = tail' fa

class Snocable f a where
  snoc' :: f a -> (a, f a)
  default snoc' :: (Headable f a, Tailable f a) => f a -> (a, f a)
  snoc' xs = (head' xs, tail' xs)

instance Snocable [] a

instance (Ord a) => Snocable Set a

class Consable f a where
  cons' :: a -> f a -> f a

instance Consable [] a where
  cons' = (:)

splitAt :: (Takeable n f a, Droppable n f a) => n -> f a -> (f a, f a)
splitAt n f = (take n f, drop n f)

halve :: forall n f a. (Sizable (f a), Integral n, Takeable n f a, Droppable n f a) => f a -> (f a, f a)
halve f = splitAt @n @f @a (size f `div` 2) f

class Arbitrary f a where
  arbitrary :: f a -> a
  default arbitrary :: f a -> a
  arbitrary = fst . arbitrarySnoc

  arbitrarySnoc :: f a -> (a, f a)
  default arbitrarySnoc :: (Unable f, Mkable f a) => f a -> (a, f a)
  arbitrarySnoc = second mk . arbitrarySnoc @[] . un

  arb :: f a -> Maybe a
  default arb :: (Foldable f) => f a -> Maybe a
  arb as = case F.toList as of
    [] -> Nothing
    _ -> Just (arbitrary as)

instance Arbitrary [] a where
  arbitrarySnoc [] = error "arbitrary: no elements"
  arbitrarySnoc (a : as) = (a, as)

instance (Ord a) => Arbitrary Set a

instance Arbitrary Seq a

instance (Ord k) => Arbitrary (Map k) v where
  arbitrarySnoc = bimap snd mkMap . arbitrarySnoc . unMap

instance (Ord k) => Arbitrary (MinQ k) a where
  arbitrarySnoc as = let ((_, a), as') = PQ.deleteFindMin as in (a, as')

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (a, b) c = c >= a && c <= b

data RangeSize = Incl | Excl deriving (Show, Eq)

rlen :: (Num a) => RangeSize -> (a, a) -> a
rlen Incl (a, b) = b - a + 1
rlen Excl (a, b) = b - a

type a |-| b = RangeOf a

data RangeOf2 a b = RangeOf RangeSize (a, b) deriving (Show, Eq)

type RangeOf a = RangeOf2 a a

instance Functor (RangeOf2 a) where
  fmap f (RangeOf rs (a, b)) = RangeOf rs (fmap f (a, b))

instance Bifunctor RangeOf2 where
  bimap f g (RangeOf rs (a, b)) = RangeOf rs (bimap f g (a, b))

instance Biapplicative RangeOf2 where
  bipure a b = RangeOf Excl (bipure a b)
  (RangeOf rs (f, g)) <<*>> (RangeOf rs0 (a, b)) = RangeOf rs0 ((f, g) <<*>> (a, b))

instance (Ord a) => Ord (RangeOf a) where
  compare (RangeOf _ (a1, b1)) (RangeOf _ (a2, b2)) = compare (a1, b1) (a2, b2)

instance (Ord a) => Memberable a (RangeOf a) where
  x ∈ (RangeOf Incl (a, b)) = x >= a && x <= b
  x ∈ (RangeOf Excl (a, b)) = x >= a && x < b

type family MagnitudeF a :: *

type instance MagnitudeF [_] = Integer

type instance MagnitudeF (NonEmpty _) = Integer

type instance MagnitudeF (Set _) = Integer

type instance MagnitudeF (Seq _) = Integer

type instance MagnitudeF (Vector _) = Integer

type instance MagnitudeF (Map _ _) = Integer

type instance MagnitudeF (a, a) = a

type instance MagnitudeF (RangeOf a) = a

type instance MagnitudeF Integer = Integer

type instance MagnitudeF Int = Integer

data MagnitudeOp a b

type family (|.|) a b where
  (|.|) a _ = MagnitudeF a

class Magnitude a where
  (|.|) :: a -> MagnitudeF a
  default (|.|) :: (Sizable a, Integral (MagnitudeF a)) => a -> MagnitudeF a
  (|.|) = size

  magnitude :: a -> MagnitudeF a
  default magnitude :: a -> MagnitudeF a
  magnitude = (|.|)

xs |?| p = ([x | x <- xs, p x] |.|)

a |=| b = (a |.|) ≡ (b |.|)

a |≡| b = (a |.|) ≡ (b |.|)

a |≢| b = (a |.|) ≢ (b |.|)

instance (Integral a) => Magnitude (RangeOf a) where
  (|.|) (RangeOf rs ab) = rlen rs ab

instance (Integral a) => Magnitude (a, a) where
  (|.|) a = (|.|) (RangeOf Excl a)

instance Magnitude [a]

instance Magnitude (Seq a)

instance Magnitude (Set a)

instance Magnitude (Map k v)

instance Magnitude (Vector a)

instance Magnitude (NonEmpty a)

instance Magnitude Int

instance Magnitude Integer

class Transposable a where
  (⊤) :: a -> a

instance Transposable [[a]] where
  (⊤) = transpose

instance (Ord a) => Transposable (Set (a, a)) where
  (⊤) = setMap Prelude.swap

class HMirrorable a where
  (◐) :: a -> a

class VMirrorable a where
  (◓) :: a -> a

instance (Mkable Set (a, a), Num a, Ord a) => VMirrorable (Set (a, a)) where
  (◓) = setMap (second negate)

class Rotatable a where
  (↺) :: a -> a
  (↻) :: a -> a

instance (Mkable Set (a, a), Num a, Ord a) => HMirrorable (Set (a, a)) where
  (◐) = setMap (first negate)

instance (Mkable Set (a, a), Num a, Ord a) => Rotatable (Set (a, a)) where
  (↺) = setMap (\(x, y) -> (0 - y, x))
  (↻) = setMap (\(x, y) -> (y, 0 - x))

class Bimaximum a where
  bimaximum :: (Foldable f) => f a -> a
  default bimaximum :: (Bimaximum [a], Foldable f) => f a -> a
  bimaximum as = bimaximum $ F.toList as

instance (Ord a) => Bimaximum (a, a) where
  bimaximum as =
    let l = F.toList as
     in (maximum (fst <$> l), maximum (snd <$> l))

class Biminimum a where
  biminimum :: (Foldable f) => f a -> a
  default biminimum :: (Biminimum [a], Foldable f) => f a -> a
  biminimum as = biminimum $ F.toList as

instance (Ord a) => Biminimum (a, a) where
  biminimum as =
    let l = F.toList as
     in (minimum (fst <$> l), minimum (snd <$> l))

class Trifunctor (f :: Type -> Type -> Type -> Type) where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'
  third :: (c -> c') -> f a b c -> f a b c'
  default third :: (c -> c') -> f a b c -> f a b c'
  third = trimap id id

instance Trifunctor (,,) where
  trimap f g h (a, b, c) = (f a, g b, h c)

three :: (Trifunctor f) => (a -> b) -> f a a a -> f b b b
three f = trimap f f f

class (Trifunctor f) => Thd f where
  thd :: f a b c -> c

instance Thd (,,) where
  thd (_, _, c) = c

class Middle f where
  middle :: f a -> a

instance Middle [] where
  middle xs = xs L.!! (length xs `div` 2)

snoc :: [a] -> (a, [a])
snoc (x : xs) = (x, xs)

class ZipWithable f where
  (⤊) :: (ZipWithable f) => (f a, f b) -> (a -> b -> c) -> f c

  (⤊*) :: (Integral i, Mkable f a, Semigroup (f a), Foldable f, Functor f) => f a -> f i -> f a
  xs ⤊* ns = ((Ŀ (<>) ((⤊) @f (fromIntegral <$> ns, xs) (\n x -> mk @f (replicate n x)))) !>)

instance ZipWithable [] where
  (a, b) ⤊ f = zipWith f a b

instance ZipWithable NonEmpty where
  (a, b) ⤊ f = NonEmpty.zipWith f a b

instance (Ord a) => MonoFunctor (Set a) where
  omap = S.map

class Uniqueable f a where
  uniq :: f a -> f a

instance (Ord a) => Uniqueable Set a where
  uniq = id

instance (Eq a) => Uniqueable [] a where
  uniq = nub

instance (Eq a) => Uniqueable Seq a where
  uniq = mk ∘ nub ∘ un

instance (Eq a, Ord k) => Uniqueable (MinQ k) a where
  uniq NullQ = (∅)
  uniq q@(_ :<! NullQ) = q
  uniq ((la, a) :<! q'@((_, b) :<! q))
    | a ≡ b = uniq (qInsert (const la) a q)
    | otherwise = qInsert (const la) a (uniq q')
  uniq q = q

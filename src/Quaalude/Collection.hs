{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quaalude.Collection where

import Control.Lens (element, (.~))
import Data.Array qualified as A
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
import Data.List.Split qualified as LS
import Data.Map.Strict qualified as M
import Data.PQueue.Prio.Min qualified as PQ
import Data.Sequence qualified as SQ
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Quaalude.Alias
import Quaalude.Unary
import Relude.Unsafe qualified as U
import Prelude hiding (drop, filter, splitAt, take)

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
  mk = id

instance Mkable V.Vector a where
  mk = mkVec

instance Mkable Seq a where
  mk = mkSeq

instance (Ord a) => Mkable Set a where
  mk = mkSet

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
  un = id

instance Unable V.Vector where
  un = unVec

instance Unable Set where
  un = unSet

instance Unable (Map k) where
  un = fmap snd . unMap

instance Unable Seq where
  un = unSeq

class UnableKey f where
  unKey :: f k v -> [(k, v)]

instance UnableKey Map where
  unKey = unMap

instance UnableKey BM.Bimap where
  unKey = unBimap

class Convable a c where
  co :: a -> c

instance (Convable a b, Convable c d) => Convable (a, c) (b, d) where
  co (a, c) = (co a, co c)

instance {-# INCOHERENT #-} Convable a [a] where
  co = pure

instance {-# INCOHERENT #-} (Unable f, Convable [a] b) => Convable (f a) b where
  co = co . un

instance {-# INCOHERENT #-} (Ord b, Convable a [b]) => Convable a (Set b) where
  co = mkOrd . co

instance {-# INCOHERENT #-} (Ord k) => Convable [(k, v)] (Map k v) where
  co = mkMap

instance {-# INCOHERENT #-} (Unable f) => Convable (f a) [a] where
  co = un

instance {-# INCOHERENT #-} (UnableKey f) => Convable (f k v) [(k, v)] where
  co = unKey

instance {-# OVERLAPPING #-} (Ord a) => Convable [a] (Set a) where
  co = mkSet

instance {-# INCOHERENT #-} (Convable a [b], Mkable f b) => Convable a (f b) where
  co = mk . co

instance {-# OVERLAPPING #-} Convable (Map k v) [(k, v)] where
  co = unMap

instance {-# INCOHERENT #-} (Mkable f a) => Convable [a] (f a) where
  co = mk

instance {-# INCOHERENT #-} (MkableKey f, Ord k) => Convable [(k, v)] (f k v) where
  co = mkKey

instance {-# OVERLAPPABLE #-} (Convable a b, Convable b c) => Convable a c where
  co a = co (co a :: b)

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

(<-|) :: (Ord a) => Set a -> a -> Set a
(<-|) = flip S.insert

(|->) :: (Ord a) => a -> Set a -> Set a
(|->) = S.insert

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = (S.\\)

class Sizable a where
  size :: (Integral i) => a -> i

instance Sizable [a] where
  size = fromIntegral . L.length

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

class Memberable a b where
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

instance (Ord k) => Gettable Map k v where
  (|!) = (M.!)

instance (A.Ix i) => Gettable A.Array i e where
  (|!) = (A.!)

class MaybeGettable f k v where
  (|?) :: f k v -> k -> Maybe v

instance (Ord k) => MaybeGettable Map k v where
  (|?) = flip M.lookup

instance (A.Ix i) => MaybeGettable A.Array i e where
  a |? i = if i ∈ a then Just (a |! i) else Nothing

class ValueGettable f k v where
  (|?>) :: f -> v -> [k]
  (|!>) :: f -> v -> k
  default (|!>) :: f -> v -> k
  xs |!> v = U.head (xs |?> v)

instance (Eq a) => ValueGettable [a] Int a where
  l |?> a = L.elemIndices a l

instance (Ord k, Ord v) => ValueGettable (Map k v) k v where
  m |?> v = swapcat m |? v ? []

class Settable f k v where
  (|.) :: f k v -> (k, v) -> f k v

instance (Ord k) => Settable Map k v where
  m |. (k, v) = M.insert k v m

instance (A.Ix i) => Settable A.Array i e where
  a |. (i, e) = a A.// [(i, e)]

instance (Ord k) => Settable PQ.MinPQueue k v where
  m |. (k, v) = PQ.insert k v m

class Modifiable f k v where
  (|~) :: f k v -> (k, v -> v) -> f k v

instance (Ord k) => Modifiable Map k v where
  m |~ (k, f) = M.adjust f k m

instance (A.Ix i) => Modifiable A.Array i e where
  a |~ (i, f)
    | i ∈ a = a A.// [(i, f (a |! i))]
    | otherwise = a

class Keysable f k where
  keys :: f -> [k]

instance (Ord k) => Keysable (Map k v) k where
  keys = M.keys

class Valuesable f k v where
  values :: f k v -> [v]

instance Valuesable Map k v where
  values = M.elems

class Deletable m k where
  delete :: k -> m -> m
  default delete :: k -> m -> m
  delete = flip (|\)
  (|\) :: m -> k -> m
  default (|\) :: m -> k -> m
  (|\) = flip delete

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

(∖) :: (Ord a) => Set a -> Set a -> Set a
(∖) = S.difference

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

setMap :: (Ord b) => (a -> b) -> Set a -> Set b
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

(|>) :: Seq a -> a -> Seq a
(|>) = (SQ.|>)

(><) :: Seq a -> Seq a -> Seq a
(><) = (SQ.><)

(>/<) :: (Eq a) => Seq a -> a -> Seq a
s >/< a = foldl' (flip SQ.deleteAt) s (SQ.elemIndicesL a s)

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

type Q = PQ.MinPQueue

pattern NullQ <- (nullQ -> True)

pattern ka :<! q <- (PQ.deleteFindMin -> (ka, q))
  where
    ka :<! q = q |. ka

pattern a :<!! q <- (first snd . PQ.deleteFindMin -> (a, q))

(<!) :: (Ord k) => PQ.MinPQueue k a -> ((k, a), PQ.MinPQueue k a)
(<!) = PQ.deleteFindMin

(<!!) :: (Ord k) => PQ.MinPQueue k a -> (a, PQ.MinPQueue k a)
(<!!) = first snd . PQ.deleteFindMin

mkArray :: (A.Ix i) => (i, i) -> [(i, e)] -> A.Array i e
mkArray = A.array

uhead :: [a] -> a
uhead = U.head

utail :: [a] -> [a]
utail = U.tail

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
  default swapWith :: (Applicative m, Ord b, Monoid (m a), UnableKey f, MkWithable f) => (m a -> m a -> m a) -> f a b -> f b (m a)
  swapWith f = mkWith f . fmap (second pure . Prelude.swap) . unKey

instance (Ord k, Ord v) => Swappable Map k v

instance (Ord k, Ord v, Applicative m, Monoid (m k)) => SwapWithable Map m k v

swapcat :: (SwapWithable f m a b, Semigroup (m a)) => f a b -> f b (m a)
swapcat = swapWith (<>)

type MinQ = PQ.MinPQueue

class Filterable f a where
  filter :: (a -> Bool) -> f a -> f a

(|-?->) :: (Filterable f a) => f a -> (a -> Bool) -> f a
(|-?->) = flip filter

instance Filterable [] a where
  filter = L.filter

instance Filterable Set a where
  filter = S.filter

instance Filterable Seq a where
  filter = SQ.filter

instance Filterable (Map k) v where
  filter = M.filter

class Takeable n f a where
  take :: n -> f a -> f a

instance (Integral n) => Takeable n [] a where
  take n = L.take (fromIntegral n)

instance (Integral n) => Takeable n V.Vector a where
  take n = V.take (fromIntegral n)

instance (Integral n) => Takeable n SQ.Seq a where
  take n = SQ.take (fromIntegral n)

class Droppable n f a where
  drop :: n -> f a -> f a

instance (Integral n) => Droppable n [] a where
  drop n = L.drop (fromIntegral n)

instance (Integral n) => Droppable n V.Vector a where
  drop n = V.drop (fromIntegral n)

instance (Integral n) => Droppable n Seq a where
  drop n = SQ.drop (fromIntegral n)

splitAt :: (Integral n, Takeable n f a, Droppable n f a) => n -> f a -> (f a, f a)
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

instance Arbitrary [] a where
  arbitrarySnoc [] = error "arbitrary: no elements"
  arbitrarySnoc (a : as) = (a, as)

instance (Ord a) => Arbitrary Set a

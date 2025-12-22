module Quaalude.BoundedSet where

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
import Quaalude.Collection
import Quaalude.Tracers
import Quaalude.Unary
import Relude.Unsafe qualified as U
import Prelude hiding (drop, filter, splitAt, take)

class Originable f a where
  origin :: a
  toOrigin :: f a -> f a

instance (Ord a, Num a) => Originable BoundedSet (a, a) where
  origin = (0, 0)
  toOrigin bs@(BoundedSet (minX, minY) _ _) = omap (\(x, y) -> (x - minX, y - minY)) bs

class Bounded1 f a where
  minBound1 :: f a -> a
  maxBound1 :: f a -> a

instance Bounded1 BoundedSet a where
  minBound1 (BoundedSet min _ _) = min
  maxBound1 (BoundedSet _ max _) = max

data BoundedSet a = BoundedSet a a (Set a) deriving (Eq, Ord, Show)

type instance MagnitudeF (BoundedSet _) = Integer

instance Magnitude (BoundedSet a)

instance Foldable BoundedSet where
  foldMap f (BoundedSet _ _ s) = foldMap f s

instance (Transposable (Set (a, a))) => Transposable (BoundedSet (a, a)) where
  (⊤) (BoundedSet (minX, minY) (maxX, maxY) cs) = BoundedSet (minY, minX) (maxY, maxX) (cs ⊤)

instance (Ord a) => Insertable BoundedSet a where
  a |-> BoundedSet min0 max0 s = BoundedSet (min min0 a) (max max0 a) (a |-> s)

instance (Biminimum a, Bimaximum a, Ord a) => Filterable BoundedSet a where
  BoundedSet min max s |-?-> f = mk ∘ un $ s |-?-> f

instance (Ord a, Biminimum a, Bimaximum a) => Semigroup (BoundedSet a) where
  (BoundedSet min0 max0 s0) <> (BoundedSet min1 max1 s1) = BoundedSet (biminimum [min0, min1]) (bimaximum [max0, max1]) (s0 <> s1)

instance (Ord a, Biminimum a, Bimaximum a, Monoid a) => Monoid (BoundedSet a) where
  mempty = BoundedSet mempty mempty mempty

instance SetMap BoundedSet where
  setMap f (BoundedSet min0 max0 s) = BoundedSet (f min0) (f max0) (setMap f s)

type instance Element (BoundedSet a) = a

instance (Ord a) => MonoFunctor (BoundedSet a) where
  omap f (BoundedSet min0 max0 s) = BoundedSet (f min0) (f max0) (omap f s)

instance (Biminimum a, Bimaximum a, Ord a) => Mkable BoundedSet a where
  mk as = BoundedSet (biminimum as) (bimaximum as) (mkSet as)

instance Unable BoundedSet where
  un (BoundedSet _ _ cs) = un cs

instance (Sizable (Set a)) => Sizable (BoundedSet a) where
  size (BoundedSet _ _ s) = size s

instance (Memberable a (Set a)) => Memberable a (BoundedSet a) where
  a ∈ (BoundedSet _ _ s) = a ∈ s
  a ∉ (BoundedSet _ _ s) = a ∉ s

instance (Ord a, Biminimum a, Bimaximum a, Unionable (Set a)) => Unionable (BoundedSet a) where
  (BoundedSet min0 max0 s0) ∪ (BoundedSet min1 max1 s1) = mk $ un (s0 ∪ s1)

instance (Ord a, Biminimum a, Bimaximum a, Intersectable (Set a)) => Intersectable (BoundedSet a) where
  (BoundedSet min0 max0 s0) ∩ (BoundedSet min1 max1 s1) = mk $ un (s0 ∩ s1)

instance (Arbitrary Set a, Biminimum a, Bimaximum a, Ord a) => Arbitrary BoundedSet a where
  arbitrary (BoundedSet _ _ a) = arbitrary @Set a

instance (Num a, Ord a) => HMirrorable (BoundedSet (a, a)) where
  (◐) (BoundedSet (minX, minY) (maxX, maxY) cs) = mk ∘ un $ (setMap (first negate) cs)

instance (Num a, Ord a) => VMirrorable (BoundedSet (a, a)) where
  (◓) (BoundedSet (minX, minY) (maxX, maxY) cs) = mk . un $ (setMap (second negate) cs)

instance (Num a, Ord a) => Rotatable (BoundedSet (a, a)) where
  (↺) (BoundedSet (minX, minY) (maxX, maxY) cs) = mk ∘ un $ (setMap (\(x, y) -> (0 - y, x)) cs)
  (↻) (BoundedSet (minX, minY) (maxX, maxY) cs) = mk ∘ un $ (setMap (\(x, y) -> (y, 0 - x)) cs)

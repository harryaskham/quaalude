module Quaalude.MaxSet where

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

data MaxSet a = MaxSet a (Set a) deriving (Eq, Ord, Show)

type instance MagnitudeF (MaxSet _) = Integer

instance Magnitude (MaxSet a)

instance Foldable MaxSet where
  foldMap f (MaxSet _ s) = foldMap f s

instance (Transposable (Set (a, a))) => Transposable (MaxSet (a, a)) where
  (⊤) (MaxSet (maxX, maxY) cs) = MaxSet (maxY, maxX) (cs ⊤)

instance (Num a, Ord a) => HMirrorable (MaxSet (a, a)) where
  (◐) (MaxSet (maxX, maxY) cs) = MaxSet (maxX, maxY) (setMap (first (maxX -)) cs)

instance (Ord a) => Insertable MaxSet a where
  a |-> MaxSet m s = MaxSet (max m a) (a |-> s)

instance (Bimaximum a, Ord a) => Filterable MaxSet a where
  MaxSet m s |-?-> f =
    let s' = s |-?-> f
     in if m ∈ s' then MaxSet m s' else mk (un s')

instance (Ord a, Bimaximum a) => Semigroup (MaxSet a) where
  (MaxSet m0 s0) <> (MaxSet m1 s1) = MaxSet (bimaximum [m0, m1]) (s0 <> s1)

instance (Ord a, Bimaximum (a, a), Num a) => Monoid (MaxSet (a, a)) where
  mempty = MaxSet (0, 0) (∅)

instance SetMap MaxSet where
  setMap f (MaxSet m s) = MaxSet (f m) (setMap f s)

type instance Element (MaxSet a) = a

instance (Ord a) => MonoFunctor (MaxSet a) where
  omap f (MaxSet m s) = MaxSet (f m) (omap f s)

instance (Bimaximum a, Ord a) => Mkable MaxSet a where
  mk as = MaxSet (bimaximum as) (mkSet as)

instance Unable MaxSet where
  un (MaxSet _ cs) = un cs

instance (Sizable (Set a)) => Sizable (MaxSet a) where
  size (MaxSet _ s) = size s

instance (Memberable a (Set a)) => Memberable a (MaxSet a) where
  a ∈ (MaxSet _ s) = a ∈ s
  a ∉ (MaxSet _ s) = a ∉ s

instance (Ord a, Bimaximum a, Unionable (Set a)) => Unionable (MaxSet a) where
  (MaxSet m0 s0) ∪ (MaxSet m1 s1) = mk $ un (s0 ∪ s1)

instance (Ord a, Bimaximum a, Intersectable (Set a)) => Intersectable (MaxSet a) where
  (MaxSet m0 s0) ∩ (MaxSet m1 s1) = mk $ un (s0 ∩ s1)

instance (Arbitrary Set a, Bimaximum a, Ord a) => Arbitrary MaxSet a where
  arbitrary (MaxSet _ a) = arbitrary @Set a

instance (Num a, Ord a) => VMirrorable (MaxSet (a, a)) where
  (◓) (MaxSet (maxX, maxY) cs) = MaxSet (maxX, maxY) (setMap (second (maxY -)) cs)

instance (Num a, Ord a) => Rotatable (MaxSet (a, a)) where
  (↺) (MaxSet (maxX, maxY) cs) = MaxSet (maxY, maxX) (setMap (\(x, y) -> (maxY - y, x)) cs)
  (↻) (MaxSet (maxX, maxY) cs) = MaxSet (maxY, maxX) (setMap (\(x, y) -> (y, maxX - x)) cs)

module Quaalude.LossQ where

import Control.Applicative
import Data.Foldable qualified as F
import Data.MonoTraversable
import Data.Set qualified as S
import Quaalude.Alias
import Quaalude.Collection
import Quaalude.Tracers
import Quaalude.Unary
import Quaalude.Util
import Text.Show
import Prelude hiding (drop, filter, splitAt, take)

type family LossF a :: *

type instance LossF Integer = Integer

type instance LossF Int = Int

type instance LossF Double = Double

type instance LossF (a, b) = (a, b)

type instance LossF (a, b, c) = (a, b, c)

data LossFn a = LossFn (a -> LossF a)

instance (a ~ LossF a) => Semigroup (LossFn a) where
  LossFn a <> LossFn b = LossFn (a ∘ b)

instance (a ~ LossF a) => Monoid (LossFn a) where
  mempty = LossFn id

instance Show (LossFn a) where
  show _ = "LossFn"

instance Eq (LossFn a) where
  _ == _ = True

instance Ord (LossFn a) where
  compare _ _ = EQ

data LossQ a where
  LossQ :: (Ord (LossF a)) => LossFn a -> MinQ (LossF a) a -> LossQ a

deriving instance (Eq a) => Eq (LossQ a)

deriving instance (Ord a) => Ord (LossQ a)

instance (Ord (LossF a), Show (LossF a), Show a) => Show (LossQ a) where
  show (LossQ _ q) = "LossQ " <> Text.Show.show q

type instance Element (LossQ a) = a

instance (Semigroup (MinQ (LossF a) a)) => Semigroup (LossQ a) where
  LossQ _ a <> LossQ loss b = LossQ loss (a <> b)

instance (Ord (LossF a), Monoid (LossFn a), Monoid (MinQ (LossF a) a)) => Monoid (LossQ a) where
  mempty = LossQ mempty mempty

instance (Sizable (MinQ (LossF a) a)) => Sizable (LossQ a) where
  size (LossQ loss q) = size q

instance (a ~ LossF a, Ord a) => Mkable LossQ a where
  mk xs = let loss = id in LossQ (LossFn loss) (mkQ [(loss a, a) | a <- xs])

instance Unable LossQ where
  un (LossQ loss q) = F.toList q

instance Foldable LossQ where
  foldMap f (LossQ loss (q :: (MinQ (LossF a) a))) = foldMap @(MinQ (LossF a)) f q

instance (Arbitrary (MinQ (LossF a)) a, Ord (LossF a)) => Arbitrary LossQ a where
  arbitrarySnoc (LossQ loss xs) =
    let (a, as) = arbitrarySnoc @(MinQ (LossF a)) @a xs
     in (a, LossQ loss as)
  arb (LossQ loss xs) = case xs of
    NullQ -> Nothing
    ((_, x) :<! _) -> Just x

instance (Integral i, Takeable i (MinQ (LossF a)) a) => Takeable i LossQ a where
  take n (LossQ loss q) = LossQ loss (take n q)

instance (Ord (LossF a)) => Insertable LossQ a where
  a |-> (LossQ (LossFn loss) q) = LossQ (LossFn loss) (qInsert loss a q)

instance (Eq a, Ord (LossF a)) => Uniqueable LossQ a where
  uniq (LossQ (LossFn loss) q) = LossQ (LossFn loss) (uniq q)

instance (Eq a, Ord (LossF a)) => Filterable LossQ a where
  (LossQ (LossFn loss) q) |-?-> f = LossQ (LossFn loss) (q |-?-> f)

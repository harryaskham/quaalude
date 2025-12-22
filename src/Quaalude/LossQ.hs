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

data LossFn k a = LossFn (a -> k)

instance Monoid (LossFn a a) where
  mempty = LossFn id

instance Show (LossFn k a) where
  show _ = "LossFn"

instance Eq (LossFn k a) where
  _ == _ = True

instance Ord (LossFn k a) where
  compare _ _ = EQ

data LossQ k a = LossQ (LossFn k a) (MinQ k a)

instance (Ord k, Show k, Show a) => Show (LossQ k a) where
  show (LossQ _ q) = "LossQ " <> Text.Show.show q

type instance Element (LossQ k a) = a

instance (Semigroup (MinQ k a)) => Semigroup (LossQ k a) where
  LossQ _ a <> LossQ loss b = LossQ loss (a <> b)

instance (Monoid (LossFn k a), Monoid (MinQ k a)) => Monoid (LossQ k a) where
  mempty = LossQ mempty mempty

instance (Sizable (MinQ k a)) => Sizable (LossQ k a) where
  size (LossQ loss q) = size q

instance (Ord a) => Mkable (LossQ a) a where
  mk xs = let loss = id in LossQ (LossFn loss) (mkQ [(loss a, a) | a <- xs])

instance (Unable f, Ord k) => Unable (LossQ k) where
  un (LossQ loss q) = F.toList q

instance (Ord k) => Foldable (LossQ k) where
  foldMap f (LossQ loss q) = foldMap f q

instance (Arbitrary (MinQ k) a, Ord k) => Arbitrary (LossQ k) a where
  arbitrarySnoc (LossQ loss xs) =
    let (a, as) = arbitrarySnoc @(MinQ k) @a xs
     in (a, LossQ loss as)
  arb (LossQ loss xs) = case xs of
    NullQ -> Nothing
    ((_, x) :<! _) -> Just x

instance (Integral i, Takeable i (MinQ k) q) => Takeable i (LossQ k) q where
  take n (LossQ loss q) = LossQ loss (take n q)

instance (Ord k) => Insertable (LossQ k) a where
  a |-> (LossQ (LossFn loss) q) = LossQ (LossFn loss) (qInsert loss a q)

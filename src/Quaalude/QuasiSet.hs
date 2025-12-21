module Quaalude.QuasiSet where

import Data.Foldable qualified as F
import Data.Set qualified as S
import Quaalude.Alias
import Quaalude.Collection
import Control.Applicative
import Control.Applicative
import Data.MonoTraversable
import Quaalude.Tracers
import Quaalude.Unary
import Quaalude.Util
import Prelude hiding (drop, filter, splitAt, take)

data QuasiSet f a = QuasiSet (f a)

deriving instance (Show (f a)) => Show (QuasiSet f a)

deriving instance (Eq (f a)) => Eq (QuasiSet f a)

deriving instance (Ord (f a)) => Ord (QuasiSet f a)

type instance Element (QuasiSet f a) = a

instance (Semigroup (f a)) => Semigroup (QuasiSet f a) where
  QuasiSet a <> QuasiSet b = QuasiSet (a <> b)

instance (Monoid (f a)) => Monoid (QuasiSet f a) where
  mempty = QuasiSet mempty

instance (Functor f) => Functor (QuasiSet f) where
  fmap f (QuasiSet a) = QuasiSet (f <$> a)

instance Applicative (QuasiSet []) where
  pure = QuasiSet ∘ pure
  QuasiSet f <*> QuasiSet a = QuasiSet (f <*> a)

instance (Alternative f, Applicative (QuasiSet f)) => Alternative (QuasiSet f) where
  empty = QuasiSet empty
  QuasiSet f <|> QuasiSet a = QuasiSet (f Control.Applicative.<|> a)

instance Monad (QuasiSet []) where
  QuasiSet a >>= f = mconcat (f <$> a)

instance (Sizable (f a)) => Sizable (QuasiSet f a) where
  size (QuasiSet a) = size a

instance (Mkable f a, Foldable f, Eq a) => Mkable (QuasiSet f) a where
  mk = QuasiSet ∘ mk ∘ nub

instance (Unable f) => Unable (QuasiSet f) where
  un (QuasiSet a) = un a

instance (Foldable f) => Foldable (QuasiSet f) where
  foldMap f (QuasiSet a) = foldMap f a

instance (Unable f, Mkable f a, Foldable f, Eq a) => Arbitrary (QuasiSet f) a

instance (Integral i, Takeable i f a) => Takeable i (QuasiSet f) a where
  take n (QuasiSet a) = QuasiSet (take n a)

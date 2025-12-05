{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Quaalude.Unary where

import Control.Monad.Memo
import Data.Foldable qualified as F
import Data.HList

class a ? b

infixl 4 ?

type family Ѓ a f where
  Ѓ a (?) = Maybe a

class (Monoid m) => UnMonoid m a where
  unMonoid :: m -> a

(|=<) :: (Monoid (m a), Monad f, Foldable f) => (b -> f (m a)) -> f b -> m a
f |=< ms = foldl' (<>) mempty $ f =<< ms

infixr 1 |=<

instance (Num a) => UnMonoid (Sum a) a where
  unMonoid = getSum

instance (Num a) => UnMonoid (Product a) a where
  unMonoid = getProduct

instance (Semigroup a) => Semigroup (Memo k v a) where
  a <> b = (<>) <$> a <*> b

instance (Monoid a) => Monoid (Memo k v a) where
  mempty = return mempty

instance (Monoid a) => UnMonoid (Memo k v a) a where
  unMonoid = startEvalMemo

instance (Num a, Monoid (m a), UnMonoid (m a) a) => UnMonoid [m a] a where
  unMonoid = unMonoid . mconcat

instance {-# OVERLAPPING #-} (Num a) => UnMonoid [Σ [Π a]] a where
  unMonoid = getΣ . mconcat . (fmap . fmap $ mconcat >>> getΠ)

instance {-# OVERLAPPABLE #-} (UnMonoid a b, Monoid b) => UnMonoid [a] b where
  unMonoid = mconcat . fmap unMonoid

instance
  (UnMonoid (m a) a, UnMonoid (m' a') a') =>
  UnMonoid (m a, m' a') (a, a')
  where
  unMonoid = bimap unMonoid unMonoid

-- instance {-# OVERLAPPABLE #-} (Functor m, Functor m', UnMonoid (m' a) a, UnMonoid (m a) a) => UnMonoid [m' [m a]] a where
--  unMonoid = (fmap . fmap $ unMonoid @(m a) @a . mconcat) >>> (unMonoid @(m' a) @a . mconcat)

-- instance {-# OVERLAPPING #-} (UnMonoid (m a) b) => UnMonoid [m a] b where
--   unMonoid = unMonoid . mconcat

-- instance {-# OVERLAPPABLE #-} (UnMonoid a [b], UnMonoid b c) => UnMonoid a c where
--   unMonoid = unMonoid @b @c . mconcat . unMonoid @a @[b]

-- instance forall (m :: * -> *) a. (UnMonoid (m a) a) => UnMonoid (HList '[]) a where
--   unMonoid _ = unMonoid (mempty @(m a))

instance
  ( UnMonoid (m a) a,
    HProxiesFD mas (AddProxy mas),
    SameLength' mas mas,
    HList2List mas (m a),
    HZipList rest rest zs,
    SameLength rest zs,
    SameLength (AddProxy rest) rest,
    HMapAux HList ConstMempty (AddProxy rest) rest,
    HMapAux HList UncurryMappend zs rest,
    mas ~ (m a) ': rest
  ) =>
  UnMonoid (HList mas) a
  where
  unMonoid = unMonoid @(m a) @a . mconcat . hList2List @mas @(m a)

-- A class that enables application of datatypes
class UnaryApply a b c where
  (˙) :: a -> b -> c

infixr 0 ˙

-- Intersections

class Intersectable a where
  (∩) :: a -> a -> a
  (⋂) :: (Foldable f) => () -> (f a -> a)
  (⋂) = const $ F.foldl1 (∩)

data UnaryIntersect = Ⴖ

instance (Foldable f, Intersectable a) => UnaryApply UnaryIntersect (f a) a where
  (˙) Ⴖ = F.foldl1 (∩)

-- Unions

class Unionable a where
  (∪) :: a -> a -> a
  (⋃) :: (Foldable f) => () -> (f a -> a)
  (⋃) = const $ F.foldl1 (∪)

data UnaryUnion = Ս

instance (Foldable f, Unionable a) => UnaryApply UnaryUnion (f a) a where
  (˙) Ս = F.foldl1 (∪)

-- Joint unary + N-ary operations
-- i.e. Σ a operates like the sum monoid, but (Σ ˙) is the sum function

newtype Σ a = Σ {getΣ :: a}
  deriving (Show)
  deriving newtype (Eq, Ord, Read, Num, Integral, Real, Enum)

deriving instance Functor Σ

instance Applicative Σ where
  pure = Σ
  liftA2 f (Σ a) (Σ b) = Σ (f a b)

instance Monad Σ where
  return = pure
  Σ a >>= f = f a

instance (Num a) => Semigroup (Σ a) where
  Σ a <> Σ b = Σ (a + b)

instance (Num a) => Monoid (Σ a) where
  mempty = Σ 0

instance (Num a) => UnMonoid (Σ a) a where
  unMonoid = getΣ

newtype Π a = Π {getΠ :: a}
  deriving (Show)
  deriving newtype (Eq, Ord, Read, Num, Integral, Real, Enum)

deriving instance Functor Π

instance Applicative Π where
  pure = Π
  liftA2 f (Π a) (Π b) = Π (f a b)

instance Monad Π where
  return = pure
  Π a >>= f = f a

instance (Num a) => Semigroup (Π a) where
  Π a <> Π b = Π (a * b)

instance (Num a) => Monoid (Π a) where
  mempty = Π 1

instance (Num a) => UnMonoid (Π a) a where
  unMonoid = getΠ

class ConsAlias u f a where
  consAlias :: u -> (f a -> a)

instance (Num a) => ConsAlias (Σ ()) [] a where
  consAlias _ = sum

instance (Num a) => ConsAlias (Π ()) [] a where
  consAlias _ = product

instance
  forall x a (f :: Type -> Type).
  (Foldable f, Num a) =>
  UnaryApply (x -> Σ x) (f a) a
  where
  (˙) _ = sum

instance
  forall x a (f :: Type -> Type).
  (Foldable f, Num a) =>
  UnaryApply (x -> Π x) (f a) a
  where
  (˙) _ = product

-- Negation

data UnaryNot = Ⴈ

instance UnaryApply UnaryNot Bool Bool where
  (˙) Ⴈ = not

ⴈ :: Bool -> Bool
ⴈ = not

-- Folding

data FoldLeft

data FoldRight

data InitAcc

data Init1

type family FoldAccF init acc a where
  FoldAccF InitAcc acc _ = acc
  FoldAccF Init1 _ a = a

type family FoldAccFnF dir init acc a where
  FoldAccFnF FoldLeft init acc a = (FoldAccF init acc a -> a -> FoldAccF init acc a)
  FoldAccFnF FoldRight init acc a = (a -> FoldAccF init acc a -> FoldAccF init acc a)

type family GetFoldableF m a where
  GetFoldableF m a = m a

type family FoldFnF dir init m acc a where
  FoldFnF dir init m acc a = FoldAccFnF dir init acc a -> FoldAccF init acc a -> GetFoldableF m a -> FoldAccF init acc a

data UnaryFold dir init (m :: Type -> Type) acc a
  = UnaryFold (FoldAccFnF dir init acc a) (FoldAccF init acc a) (GetFoldableF m a)

data family Fold dir init (m :: Type -> Type) acc a

data instance Fold FoldLeft InitAcc m acc a = Ł (FoldAccFnF FoldLeft InitAcc acc a) (FoldAccF InitAcc acc a) (GetFoldableF m a)

data instance Fold FoldRight InitAcc m acc a = Ɍ (FoldAccFnF FoldRight InitAcc acc a) (FoldAccF InitAcc acc a) (GetFoldableF m a)

data instance Fold FoldLeft Init1 m acc a = Ŀ (FoldAccFnF FoldLeft Init1 acc a) (GetFoldableF m a)

data instance Fold FoldRight Init1 m acc a = Ṛ (FoldAccFnF FoldRight Init1 acc a) (GetFoldableF m a)

type family FoldAccDF f where
  FoldAccDF (Fold dir init m acc a) = FoldAccF init acc a

type family FoldAccFnDF f where
  FoldAccFnDF (Fold dir init m acc a) = FoldAccFnF dir init acc a

type family GetFoldableDF f where
  GetFoldableDF (Fold dir init m acc a) = GetFoldableF m a

type family FoldFnDF f where
  FoldFnDF (Fold dir init m acc a) = FoldFnF dir init m acc a

class FoldAccFn f where
  foldAccFn :: f -> FoldAccFnDF f

instance FoldAccFn (Fold FoldLeft InitAcc m acc a) where
  foldAccFn (Ł f _ _) = f

instance FoldAccFn (Fold FoldRight InitAcc m acc a) where
  foldAccFn (Ɍ f _ _) = f

instance FoldAccFn (Fold FoldLeft Init1 m acc a) where
  foldAccFn (Ŀ f _) = f

instance FoldAccFn (Fold FoldRight Init1 m acc a) where
  foldAccFn (Ṛ f _) = f

class FoldAcc f where
  foldAcc :: f -> FoldAccDF f

instance FoldAcc (Fold FoldLeft InitAcc m acc a) where
  foldAcc (Ł _ acc _) = acc

instance FoldAcc (Fold FoldRight InitAcc m acc a) where
  foldAcc (Ɍ _ acc _) = acc

instance (Foldable m) => FoldAcc (Fold FoldLeft Init1 m acc a) where
  foldAcc (Ŀ _ xs) = let (x : _) = F.toList xs in x

instance (Foldable m) => FoldAcc (Fold FoldRight Init1 m acc a) where
  foldAcc (Ṛ _ xs) = let (x : _) = F.toList xs in x

class GetFoldable f where
  getFoldable :: f -> GetFoldableDF f

instance GetFoldable (Fold FoldLeft InitAcc m acc a) where
  getFoldable (Ł _ _ xs) = xs

instance GetFoldable (Fold FoldRight InitAcc m acc a) where
  getFoldable (Ɍ _ _ xs) = xs

instance (Foldable m) => GetFoldable (Fold FoldLeft Init1 m acc a) where
  getFoldable (Ŀ _ xs) = xs

instance (Foldable m) => GetFoldable (Fold FoldRight Init1 m acc a) where
  getFoldable (Ṛ _ xs) = xs

class FoldFn f where
  foldFn :: f -> FoldFnDF f

instance (Foldable m) => FoldFn (Fold FoldLeft InitAcc m acc a) where
  foldFn _ = F.foldl'

instance (Foldable m) => FoldFn (Fold FoldRight InitAcc m acc a) where
  foldFn _ = F.foldr

instance (Foldable m) => FoldFn (Fold FoldLeft Init1 m acc a) where
  foldFn _ f _ xs = F.foldl1 f xs

instance (Foldable m) => FoldFn (Fold FoldRight Init1 m acc a) where
  foldFn _ f _ xs = F.foldr1 f xs

class ApplyFold f where
  (!>) :: f -> FoldAccDF f

instance
  ( FoldFnDF f ~ (FoldAccFnDF f -> FoldAccDF f -> GetFoldableDF f -> FoldAccDF f),
    FoldFn f,
    FoldAccFn f,
    FoldAcc f,
    GetFoldable f
  ) =>
  ApplyFold f
  where
  (!>) f =
    let doFold :: FoldFnDF f = foldFn @f f
     in doFold (foldAccFn @f f) (foldAcc @f f) (getFoldable @f f)

-- Unary forcing

data UnaryForce = Λ

instance UnaryApply UnaryForce (() -> a) a where
  (˙) Λ = ($ ())

-- Below here: experimental use of operators in prefix mode using a placeholder on the left side.

ȣ :: ()
ȣ = ()

-- Type for enabling unary prefix ops that take a placeholder as first argument.
-- Hidden as it causes confusing type errors
-- type Unary a = () -> a

ɾ :: a -> () -> a
ɾ a _ = a

-- Partially applied u which resolves to a
class IsUnary u a where
  -- Resolve the thunk
  unary :: u -> a

  -- Postfix forcing
  (⁻) :: u -> a
  (⁻) = unary

  -- Prefix forcing
  λ :: u -> a
  λ = unary

ⵉ :: forall a (f :: Type -> Type). (Foldable f, Num a) => f a -> a
ⵉ = sum

ꛛ :: forall a (f :: Type -> Type). (Foldable f, Num a) => f a -> a
ꛛ = product

{-

instance IsUnary (Unary a) a where
  unary f = f ()

instance {-# INCOHERENT #-} IsUnary a a where
  unary = id

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Eq a) => Eq u where
  a == b = (unary a :: a) == (unary b :: a)

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Ord a) => Ord u where
  a <= b = (unary a :: a) <= (unary b :: a)

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Semigroup a) => Semigroup u where
  a <> b = ɾ ((unary a :: a) <> (unary b :: a))

instance {-# OVERLAPPABLE #-} (u ~ Unary a, IsUnary u a, Monoid a) => Monoid u where
  mempty = ɾ (mempty :: a)

mkUnary1 :: (IsUnary u a) => (a -> b) -> Unary (u -> b)
mkUnary1 f _ u = f (unary u)

mkUnary2 :: (IsUnary u a, IsUnary v b) => (a -> b -> c) -> u -> v -> Unary c
mkUnary2 f u v _ = f (unary u) (unary v)

infix 9 ¬

(¬) :: (IsUnary u Bool) => Unary (u -> Bool)
(¬) = mkUnary1 not

infixr 3 ∧

(∧) :: (IsUnary u Bool) => u -> u -> Unary Bool
(∧) = mkUnary2 (&&)

infixr 2 ∨

(∨) :: (IsUnary u Bool) => u -> u -> Unary Bool
(∨) = mkUnary2 (||)

infix 9 ⋀

(⋀) :: (Foldable t, Functor t) => Unary (t Bool -> Bool)
(⋀) _ = and

infix 9 ⋁

(⋁) :: (Foldable t, Functor t) => Unary (t Bool -> Bool)
(⋁) _ = or

infix 9 ∑

(∑) :: (Foldable t, Functor t, Num a) => Unary (t a -> a)
(∑) _ = sum

infix 9 ∏

(∏) :: (Foldable t, Functor t, Num a) => Unary (t a -> a)
(∏) _ = product

testUnary :: Bool
testUnary =
  let a :: Unary Int
      a = (∏ [2 .. 4 :: Int])
      b :: Bool
      b = a == ɾ 24
      c :: Bool
      c = λ ((∑ [1 .. 10 :: Int]) > ɾ 100)
      d :: Unary Bool
      d = (¬ c)
      e :: Unary Bool
      e = ɾ b ∨ d
      f :: Unary Bool
      f = (⋀ fmap λ [ɾ b, d, e])
   in λ f

-}

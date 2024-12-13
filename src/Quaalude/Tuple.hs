module Quaalude.Tuple where

import Data.List qualified as L

type family NTuple (n :: Nat) a where
  NTuple 0 _ = ()
  NTuple 1 a = a
  NTuple 2 a = (a, a)
  NTuple 3 a = (a, a, a)
  NTuple 4 a = (a, a, a, a)
  NTuple 5 a = (a, a, a, a, a)
  NTuple 6 a = (a, a, a, a, a, a)
  NTuple 7 a = (a, a, a, a, a, a, a)
  NTuple 8 a = (a, a, a, a, a, a, a, a)
  NTuple 9 a = (a, a, a, a, a, a, a, a, a)
  NTuple 10 a = (a, a, a, a, a, a, a, a, a, a)

type family a × b where
  a × b = (a, b)

class TupleCons a b c | a b -> c where
  tupleCons :: a -> b -> c
  tupleCons = (×)
  (×) :: a -> b -> c
  (×) = tupleCons

instance TupleCons a b (a, b) where
  (×) = (,)

(<$@>) :: (Functor f) => (a -> b -> c) -> f (a, b) -> f c
f <$@> a = uncurry f <$> a

infixr 0 <$@>

(=<<@) :: (Monad m) => (a -> b -> m c) -> m (a, b) -> m c
f =<<@ a = uncurry f =<< a

infixr 1 =<<@

($@) :: (a -> b -> c) -> (a, b) -> c
f $@ a = uncurry f a

infixr 0 $@

($$@) :: (a -> (a -> b)) -> a -> b
f $$@ a = f a a

infixr 0 $$@

(&@) :: (a, b) -> (a -> b -> c) -> c
(&@) = flip ($@)

infixl 1 &@

(&&@) :: a -> (a -> (a -> b)) -> b
(&&@) = flip ($$@)

infixl 1 &&@

aps :: (Applicative f) => (a -> b -> c) -> f a -> b -> f c
aps f as b = f <$> as <*> pure b

($<@>) :: (Applicative f) => (a -> b -> c) -> (f a, b) -> f c
f $<@> (as, b) = aps f as b

infixr 0 $<@>

(&<@>) :: (Applicative f) => (f a, b) -> (a -> b -> c) -> f c
(&<@>) = flip ($<@>)

infixl 1 &<@>

dup :: a -> (a, a)
dup a = (a, a)

class Trifunctor (f :: Type -> Type -> Type -> Type) where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'
  third :: (c -> c') -> f a b c -> f a b c'
  default third :: (c -> c') -> f a b c -> f a b c'
  third = trimap id id

instance Trifunctor (,,) where
  trimap f g h (a, b, c) = (f a, g b, h c)

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

instance (Num a) => Num (a, a) where
  (a, b) + (c, d) = (a + c, b + d)
  (a, b) - (c, d) = (a - c, b - d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = (signum a, signum b)
  fromInteger n = (fromInteger n, fromInteger n)

instance (Enum (a, a), Real (a, a), Integral a) => Integral (a, a) where
  toInteger _ = error "toInteger on tuple"
  quotRem (a, b) (c, d) = (quotRem a c, quotRem b d)

class TupDrop (n :: Nat) a b where
  tupdrop :: a -> b

instance TupDrop 1 (a, b, c) (a, b) where
  tupdrop (a, b, _) = (a, b)

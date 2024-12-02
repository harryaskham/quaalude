module Quaalude.Tuple where

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

($@) :: (a -> b -> c) -> (a, b) -> c
f $@ a = uncurry f a

infixr 0 $@

(&@) :: (a, b) -> (a -> b -> c) -> c
(&@) = flip ($@)

infixl 1 &@

aps :: (Applicative f) => (a -> b -> c) -> f a -> b -> f c
aps f as b = f <$> as <*> pure b

($<@>) :: (Applicative f) => (a -> b -> c) -> (f a, b) -> f c
f $<@> (as, b) = aps f as b

infixr 0 $<@>

(&<@>) :: (Applicative f) => (f a, b) -> (a -> b -> c) -> f c
(&<@>) = flip ($<@>)

infixl 1 &<@>

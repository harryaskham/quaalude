module Quaalude.Tuple where

type a × b = (a, b)

class TupleCons a b c | a b -> c where
  tupleCons :: a -> b -> c
  tupleCons = (×)
  (×) :: a -> b -> c
  (×) = tupleCons

instance TupleCons a b (a × b) where
  (×) = (,)

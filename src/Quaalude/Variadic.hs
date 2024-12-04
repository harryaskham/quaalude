module Quaalude.Variadic where

import GHC.TypeLits
import Quaalude.Compose

data a |=> b where
  Variadic ::
    (forall n. ToVariadic n a b) =>
    (forall n. (ToVariadic n a b) => ToVariadicF n a [b]) ->
    a |=> b

data a |=*-> b where
  Variadicat ::
    (forall n. ToVariadicConcat n a b) =>
    (forall n. (ToVariadicConcat n a b) => ToVariadicF n a b) ->
    a |=*-> b

type family VF ab n where
  VF (a |=> b) n = ToVariadicF n a [b]
  VF (a |=*-> b) n = ToVariadicF n a b

type family VC ab n :: Constraint where
  VC (a |=> b) n = ToVariadic n a b
  VC (a |=*-> b) n = ToVariadicConcat n a b

type n -*=| ab = (VC ab n) => VF ab n

infixr 0 -*=|

type family ToVariadicF (arity :: Nat) a b where
  ToVariadicF 0 _ b = b
  ToVariadicF arity a b = ToVariadicF (arity - 1) a (a -> b)

class ToVariadic (arity :: Nat) a b where
  variadic :: (a -> b) -> ToVariadicF arity a [b]

instance {-# OVERLAPPING #-} ToVariadic 0 a b where
  variadic _ = [] :: [b]

instance
  {-# OVERLAPPABLE #-}
  ( ToVariadic (n - 1) a b,
    ToVariadicF n a [b] ~ (a -> ComposeNF ([b] -> [b]) (ToVariadicF (n - 1) a [b])),
    ComposeN ([b] -> [b]) (ToVariadicF (n - 1) a [b])
  ) =>
  ToVariadic n a b
  where
  variadic fn a =
    let g = (fn a :)
        f = variadic @(n - 1) @a @b fn
     in g `composeN` f

class
  ( ToVariadic (arity :: Nat) a b,
    ToVariadicF arity a b ~ ComposeNF ([b] -> b) (ToVariadicF arity a [b]),
    ComposeN ([b] -> b) (ToVariadicF arity a [b]),
    Monoid b
  ) =>
  ToVariadicConcat arity a b
  where
  variadicat :: (a -> b) -> ToVariadicF arity a b
  default variadicat :: (a -> b) -> ToVariadicF arity a b
  variadicat f = composeN @([b] -> b) mconcat (variadic @arity @a @b f)

instance
  ( ToVariadic (arity :: Nat) a b,
    ComposeN ([b] -> b) (ToVariadicF arity a [b]),
    ToVariadicF arity a b ~ ComposeNF ([b] -> b) (ToVariadicF arity a [b]),
    Monoid b
  ) =>
  ToVariadicConcat arity a b

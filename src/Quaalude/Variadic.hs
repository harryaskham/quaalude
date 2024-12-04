module Quaalude.Variadic where

import GHC.TypeLits

-- type a *-> b = MakeVariadicF (a -> b)

type family MakeVariadicF (arity :: Nat) a b where
  MakeVariadicF 0 _ b = [b]
  MakeVariadicF arity a b = a -> MakeVariadicF (arity - 1) a b

class MakeVariadic (arity :: Nat) a b where
  makeVariadic :: (a -> b) -> MakeVariadicF arity a b

class
  ( MakeVariadic (arity :: Nat) a b,
    ComposeN ([b] -> b) (MakeVariadicF arity a b),
    Monoid b
  ) =>
  MakeVariadicConcat arity a b
  where
  makeVariadicConcat :: (a -> b) -> ComposeNF ([b] -> b) (MakeVariadicF arity a b)
  default makeVariadicConcat :: (a -> b) -> ComposeNF ([b] -> b) (MakeVariadicF arity a b)
  makeVariadicConcat f = composeN @([b] -> b) mconcat (makeVariadic @arity @a @b f)

instance
  ( MakeVariadic (arity :: Nat) a b,
    ComposeN ([b] -> b) (MakeVariadicF arity a b),
    Monoid b
  ) =>
  MakeVariadicConcat arity a b

instance {-# OVERLAPPING #-} MakeVariadic 0 a b where
  makeVariadic _ = [] :: [b]

instance
  {-# OVERLAPPABLE #-}
  ( MakeVariadic (n - 1) a b,
    MakeVariadicF n a b ~ (a -> ComposeNF ([b] -> [b]) (MakeVariadicF (n - 1) a b)),
    ComposeN ([b] -> [b]) (MakeVariadicF (n - 1) a b)
  ) =>
  MakeVariadic n a b
  where
  makeVariadic fn a =
    let g = (fn a :)
        f = makeVariadic @(n - 1) @a @b fn
     in g `composeN` f

-- instance (MakeVariadic (n - 1) (a -> b)) => MakeVariadic n (a -> b) where
--   makeVariadic f = \a -> (f a :) >>> makeVariadic @(n - 1) @(a -> b) f

type family ComposeNF g f where
  ComposeNF (b -> c) (a -> b) = a -> c
  ComposeNF (b -> c) b = c
  ComposeNF (b -> c) (x -> a -> b) = x -> ComposeNF (b -> c) (a -> b)

class ComposeN g f where
  composeN :: g -> f -> ComposeNF g f

instance ComposeN (b -> c) (a -> b) where
  composeN = (.)

instance (ComposeNF (b -> c) b ~ c) => ComposeN (b -> c) b where
  composeN = ($)

instance
  ((ComposeNF (b -> c) (x -> a -> b)) ~ (x -> ComposeNF (b -> c) (a -> b))) =>
  ComposeN (b -> c) (x -> a -> b)
  where
  composeN g f = \a -> composeN g (f a)

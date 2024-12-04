module Quaalude.Compose where

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

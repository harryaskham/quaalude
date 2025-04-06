module Quaalude.Lists where

import Data.HList

class Nothings l where
  nothings :: HList l

instance Nothings '[] where
  nothings = HNil

instance (Nothings xs) => Nothings (Maybe x' ': xs) where
  nothings = HCons Nothing (nothings @xs)
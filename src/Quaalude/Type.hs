{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Quaalude.Type where

import GHC.TypeLits (ErrorMessage (..), Mod, TypeError)
import GHC.TypeLits qualified as TL
import GHC.TypeNats qualified as TN

-- Naturals
data Z = Z deriving (Show, Eq)

data S n = S n deriving (Show, Eq)

-- Conversion from GHC.TypeNats
type family N (n :: Nat) where
  N 0 = Z
  N n = S (N (n TN.- 1))

-- Evidence
class (N (n TN.+ 1) ~ S (N n)) => Plus1IsSucc n

instance (N (n TN.+ 1) ~ S (N n)) => Plus1IsSucc n

-- Value-level conversions
class IsNat n where
  nat :: n
  sing :: Nat

instance IsNat Z where
  nat = Z
  sing = 0

instance (IsNat n) => IsNat (S n) where
  nat = S (nat @n)
  sing = 1 + sing @n

class FromNat (n :: Nat) where
  fromNat :: N n

instance FromNat 0 where
  fromNat = Z

instance
  ( FromNat n,
    N sn ~ S (N n)
  ) =>
  FromNat sn
  where
  fromNat = S (fromNat @n)

-- Semantic type aliases
type family If cond a (msg :: ErrorMessage) where
  If 'True a _ = a
  If 'False a msg = TypeError ('Text "Predicate failed for " ':<>: 'ShowType a ':<>: 'Text ": " ':<>: msg)

type family Not (a :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

type family a :==: b where
  a :==: a = 'True
  a :==: b = 'False

type a :/=: b = Not (a :==: b)

type Even n = If ((n `TN.Mod` 2) :==: 0) (N n) ('Text "Expected an even number")

type Odd n = If ((n `TN.Mod` 2) :==: 1) (N n) ('Text "Expected an even number")

type Without without n = If (Not (n :==: without)) n ('Text "Expected any except " :<>: TL.ShowType without)

{-
a :: Even 4
a = nat

-- a' :: Odd 4
-- a' = nat

b :: Odd 3
b = nat

c :: Without (N 3) (N 4)
c = nat

-- c' :: Without (N 3) (N 3)
-- c' = nat
-}

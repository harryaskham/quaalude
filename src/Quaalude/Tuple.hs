module Quaalude.Tuple where

import Data.HList
import Data.List qualified as L
import Data.Tuple.Solo
import GHC.TypeLits
import Quaalude.Collection
import Text.Parsec
import Prelude hiding (natVal)

type family TupConsF a as where
  TupConsF a () = (Solo a)
  TupConsF a (Solo b) = (a, b)
  TupConsF a (b, c) = (a, b, c)
  TupConsF a (b, c, d) = (a, b, c, d)
  TupConsF a (b, c, d, e) = (a, b, c, d, e)
  TupConsF a (b, c, d, e, f) = (a, b, c, d, e, f)
  TupConsF a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)
  TupConsF a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)
  TupConsF a (b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, i)
  TupConsF a (b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j)
  TupConsF a (b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j, k)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k, l)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
  TupConsF a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

type a × b = (a, b)

class TupCons a b c where
  tupCons :: a -> b -> c

instance TupCons a () (Solo a) where
  tupCons a () = MkSolo a

instance TupCons a (Solo b) (a, b) where
  tupCons a (MkSolo b) = (a, b)

instance TupCons a (b, c) (a, b, c) where
  tupCons a (b, c) = (a, b, c)

instance TupCons a (b, c, d) (a, b, c, d) where
  tupCons a (b, c, d) = (a, b, c, d)

instance TupCons a (b, c, d, e) (a, b, c, d, e) where
  tupCons a (b, c, d, e) = (a, b, c, d, e)

instance TupCons a (b, c, d, e, f) (a, b, c, d, e, f) where
  tupCons a (b, c, d, e, f) = (a, b, c, d, e, f)

instance TupCons a (b, c, d, e, f, g) (a, b, c, d, e, f, g) where
  tupCons a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)

instance TupCons a (b, c, d, e, f, g, h) (a, b, c, d, e, f, g, h) where
  tupCons a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)

instance TupCons a (b, c, d, e, f, g, h, i) (a, b, c, d, e, f, g, h, i) where
  tupCons a (b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, i)

instance TupCons a (b, c, d, e, f, g, h, i, j) (a, b, c, d, e, f, g, h, i, j) where
  tupCons a (b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j)

instance TupCons a (b, c, d, e, f, g, h, i, j, k) (a, b, c, d, e, f, g, h, i, j, k) where
  tupCons a (b, c, d, e, f, g, h, i, j, k) = (a, b, c, d, e, f, g, h, i, j, k)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l) (a, b, c, d, e, f, g, h, i, j, k, l) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l) = (a, b, c, d, e, f, g, h, i, j, k, l)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m) (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m) = (a, b, c, d, e, f, g, h, i, j, k, l, m)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n) (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)

instance TupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  tupCons a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

type family TupSnocF a where
  TupSnocF () = TypeError ('Text "TupSnocF: ()")
  TupSnocF (Solo a) = (a, ())
  TupSnocF (a, b) = (a, Solo b)
  TupSnocF (a, b, c) = (a, (b, c))
  TupSnocF (a, b, c, d) = (a, (b, c, d))
  TupSnocF (a, b, c, d, e) = (a, (b, c, d, e))
  TupSnocF (a, b, c, d, e, f) = (a, (b, c, d, e, f))
  TupSnocF (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))
  TupSnocF (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))
  TupSnocF (a, b, c, d, e, f, g, h, i) = (a, (b, c, d, e, f, g, h, i))
  TupSnocF (a, b, c, d, e, f, g, h, i, j) = (a, (b, c, d, e, f, g, h, i, j))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k) = (a, (b, c, d, e, f, g, h, i, j, k))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l) = (a, (b, c, d, e, f, g, h, i, j, k, l))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m) = (a, (b, c, d, e, f, g, h, i, j, k, l, m))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y))
  TupSnocF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z))

class TupSnoc a head tail where
  tupSnoc :: a -> (head, tail)

instance TupSnoc (Solo a) a () where
  tupSnoc (MkSolo a) = (a, ())

instance TupSnoc (a, b) a (Solo b) where
  tupSnoc (a, b) = (a, MkSolo b)

instance TupSnoc (a, b, c) a (b, c) where
  tupSnoc (a, b, c) = (a, (b, c))

instance TupSnoc (a, b, c, d) a (b, c, d) where
  tupSnoc (a, b, c, d) = (a, (b, c, d))

instance TupSnoc (a, b, c, d, e) a (b, c, d, e) where
  tupSnoc (a, b, c, d, e) = (a, (b, c, d, e))

instance TupSnoc (a, b, c, d, e, f) a (b, c, d, e, f) where
  tupSnoc (a, b, c, d, e, f) = (a, (b, c, d, e, f))

instance TupSnoc (a, b, c, d, e, f, g) a (b, c, d, e, f, g) where
  tupSnoc (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))

instance TupSnoc (a, b, c, d, e, f, g, h) a (b, c, d, e, f, g, h) where
  tupSnoc (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))

instance TupSnoc (a, b, c, d, e, f, g, h, i) a (b, c, d, e, f, g, h, i) where
  tupSnoc (a, b, c, d, e, f, g, h, i) = (a, (b, c, d, e, f, g, h, i))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j) a (b, c, d, e, f, g, h, i, j) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j) = (a, (b, c, d, e, f, g, h, i, j))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k) a (b, c, d, e, f, g, h, i, j, k) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k) = (a, (b, c, d, e, f, g, h, i, j, k))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l) a (b, c, d, e, f, g, h, i, j, k, l) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l) = (a, (b, c, d, e, f, g, h, i, j, k, l))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m) a (b, c, d, e, f, g, h, i, j, k, l, m) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m) = (a, (b, c, d, e, f, g, h, i, j, k, l, m))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) a (b, c, d, e, f, g, h, i, j, k, l, m, n) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y))

instance TupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) a (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  tupSnoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = (a, (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z))

type family TupFstF a where
  TupFstF () = TypeError ('Text "TupFstF: ()")
  TupFstF (Solo a) = a
  TupFstF (a, b) = a
  TupFstF (a, b, c) = a
  TupFstF (a, b, c, d) = a
  TupFstF (a, b, c, d, e) = a
  TupFstF (a, b, c, d, e, f) = a
  TupFstF (a, b, c, d, e, f, g) = a
  TupFstF (a, b, c, d, e, f, g, h) = a
  TupFstF (a, b, c, d, e, f, g, h, i) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = a

class TupFst a where
  tupFst :: a -> TupFstF a

instance (fst ~ TupFstF a, TupSnoc a fst tail) => TupFst a where
  tupFst = fst . tupSnoc @a @fst @tail

type family TupNthF n a where
  TupNthF 0 a = TupFstF a
  TupNthF n a = TupNthF (n - 1) (TupTailF a)

class TupNth n a where
  tupNth :: a -> TupNthF n a

instance (TupFst a) => TupNth 0 a where
  tupNth = tupFst @a

instance
  ( TupNthF (n - 1) tail ~ TupNthF n a,
    tail ~ TupTailF a,
    TupTail a tail,
    TupNth (n - 1) tail
  ) =>
  TupNth n a
  where
  tupNth = tupNth @(n - 1) @tail . tupTail @a @tail

type family TupHeadF a where
  TupHeadF () = TypeError ('Text "TupHeadF: ()")
  TupHeadF a = TupFstF a

class TupHead a head where
  tupHead :: a -> head

instance (TupSnoc a head tail) => TupHead a head where
  tupHead = fst . tupSnoc @a @head @tail

type family TupSndF a where
  TupSndF (a, b) = b

type family TupTailF a where
  TupTailF () = TypeError ('Text "TupTailF: ()")
  TupTailF a = TupSndF (TupSnocF a)

class TupTail a tail where
  tupTail :: a -> tail

instance (tail ~ TupTailF a, TupSnoc a head tail) => TupTail a tail where
  tupTail = snd . tupSnoc @a @head @tail

type family TupInitFL t where
  TupInitFL '[] = '[]
  TupInitFL '[t] = '[]
  TupInitFL (t ': ts) = (t ': (TupInitFL ts))

type TupInitF t = List2TupF (TupInitFL (Tup2ListF t))

class TupInit t where
  tupInit :: List2TupF t -> TupInitF (List2TupF t)

instance TupInit '[] where
  tupInit _ = ()

instance
  ( TupHead (List2TupF (t ': ts)) t,
    TupCons t (TupInitF (List2TupF ts)) (TupInitF (List2TupF (t ': ts))),
    TupInit ts,
    TupTail (List2TupF (t ': ts)) (List2TupF ts)
  ) =>
  TupInit (t ': ts)
  where
  tupInit t =
    tupCons @t @(TupInitF (List2TupF ts)) @(TupInitF (List2TupF (t ': ts)))
      (tupHead @(List2TupF (t ': ts)) @t t)
      (tupInit @ts (tupTail @(List2TupF (t ': ts)) @(List2TupF ts) t))

type family Tup2ListF a where
  Tup2ListF () = '[]
  Tup2ListF (Solo a) = '[a]
  Tup2ListF (a, b) = '[a, b]
  Tup2ListF (a, b, c) = '[a, b, c]
  Tup2ListF (a, b, c, d) = '[a, b, c, d]
  Tup2ListF (a, b, c, d, e) = '[a, b, c, d, e]
  Tup2ListF (a, b, c, d, e, f) = '[a, b, c, d, e, f]
  Tup2ListF (a, b, c, d, e, f, g) = '[a, b, c, d, e, f, g]
  Tup2ListF (a, b, c, d, e, f, g, h) = '[a, b, c, d, e, f, g, h]
  Tup2ListF (a, b, c, d, e, f, g, h, i) = '[a, b, c, d, e, f, g, h, i]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j) = '[a, b, c, d, e, f, g, h, i, j]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k) = '[a, b, c, d, e, f, g, h, i, j, k]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l) = '[a, b, c, d, e, f, g, h, i, j, k, l]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m) = '[a, b, c, d, e, f, g, h, i, j, k, l, m]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) = '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]

type family List2TupF a where
  List2TupF '[] = ()
  List2TupF '[a] = Solo a
  List2TupF '[a, b] = (a, b)
  List2TupF '[a, b, c] = (a, b, c)
  List2TupF '[a, b, c, d] = (a, b, c, d)
  List2TupF '[a, b, c, d, e] = (a, b, c, d, e)
  List2TupF '[a, b, c, d, e, f] = (a, b, c, d, e, f)
  List2TupF '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  List2TupF '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  List2TupF '[a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
  List2TupF '[a, b, c, d, e, f, g, h, i, j] = (a, b, c, d, e, f, g, h, i, j)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k] = (a, b, c, d, e, f, g, h, i, j, k)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l] = (a, b, c, d, e, f, g, h, i, j, k, l)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m] = (a, b, c, d, e, f, g, h, i, j, k, l, m)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)
  List2TupF '[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z] = (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)
  List2TupF e = TypeError ('Text "List2TupF: " ':<>: 'ShowType e)

class Tup2List t h where
  tup2List :: t -> HList h

instance Tup2List () '[] where
  tup2List _ = HNil

instance Tup2List (Solo a) '[a] where
  tup2List (MkSolo a) = a .*. HNil

instance
  ( TupSnoc t head tail,
    Tup2List tail ltail
  ) =>
  Tup2List t (head ': ltail)
  where
  tup2List t =
    let (head, tail) = tupSnoc @t @head @tail t
     in head .*. tup2List @tail @ltail tail

class List2Tup l where
  list2Tup :: HList l -> List2TupF l

instance List2Tup '[] where
  list2Tup = const ()

instance
  ( List2Tup tail,
    TupCons head (List2TupF tail) (List2TupF (head ': tail))
  ) =>
  List2Tup (head ': tail)
  where
  list2Tup (HCons head tail) =
    tupCons @head @(List2TupF tail) @(List2TupF (head ': tail))
      head
      (list2Tup @tail tail)

(<$@>) :: (Functor f) => (a -> b -> c) -> f (a, b) -> f c
f <$@> a = uncurry f <$> a

infixr 0 <$@>

(<$$@>) :: (Functor f, Functor g) => (a -> b -> c) -> g (f (a, b)) -> g (f c)
f <$$@> a = (fmap . fmap) (uncurry f) a

infixr 0 <$$@>

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

three :: (Trifunctor f) => (a -> b) -> f a a a -> f b b b
three f = trimap f f f

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

newtype HomTup (l :: Nat) a = HomTup [a] deriving (Show)

type family ToTupF a where
  ToTupF (HomTup 0 _) = ()
  ToTupF (HomTup 1 a) = Solo a
  ToTupF (HomTup n a) = TupConsF a (ToTupF (HomTup (n - 1) a))
  ToTupF [a] = [ToTupF a]
  ToTupF a = Solo a

type a :^ n = ToTupF (HomTup n a)

class ToTup (n :: Nat) a b where
  toTup :: a -> b

instance {-# OVERLAPS #-} ToTup 0 [a] (HomTup 0 a) where
  toTup _ = HomTup []

instance
  {-# OVERLAPS #-}
  (HomTupCons a (HomTup (n - 1) a) (HomTup n a)) =>
  ToTup n [a] (HomTup n a)
  where
  toTup (a : as) = homTupCons a (HomTup @(n - 1) as)

instance
  {-# OVERLAPS #-}
  ( ToTup n [a] (HomTup n a),
    ta ~ ToTupF (HomTup n a),
    ToTup n (HomTup n a) ta
  ) =>
  ToTup n [a] ta
  where
  toTup = toTup @n @(HomTup n a) @ta . toTup @n @[a] @(HomTup n a)

instance {-# OVERLAPS #-} (ToTups a b, ToTup n b c) => ToTup n a c where
  toTup = toTup @n @b @c . toTups @a @b

instance {-# OVERLAPS #-} (ToTup n a b) => ToTups [a] [b] where
  toTups = fmap (toTup @n @a @b)

instance {-# OVERLAPS #-} ToTup 0 (HomTup 0 a) () where
  toTup _ = ()

instance
  {-# OVERLAPS #-}
  ( ToTup (n - 1) (HomTup (n - 1) a) (ToTupF (HomTup (n - 1) a)),
    TupCons a (ToTupF (HomTup (n - 1) a)) t,
    t ~ ToTupF (HomTup n a)
  ) =>
  ToTup n (HomTup n a) t
  where
  toTup (HomTup (a : as)) =
    tupCons @a @(ToTupF (HomTup (n - 1) a)) @(ToTupF (HomTup n a))
      a
      (toTup @(n - 1) @(HomTup (n - 1) a) @(ToTupF (HomTup (n - 1) a)) (HomTup @(n - 1) @a as))

type family ToTupsF a where
  ToTupsF () = ()
  ToTupsF (HList a) = List2TupF a
  ToTupsF a = a

class ToTups a b where
  toTups :: a -> b

instance (KnownNat n) => ToTups [a] [HomTup n a] where
  toTups = let n = natVal (Proxy @n) in fmap (HomTup @n) . chunksOf (fromIntegral n)

instance ToTups () () where
  toTups = const ()

instance (List2Tup l, t ~ List2TupF l) => ToTups (HList l) t where
  toTups = list2Tup @l

class MkHomTup n a b where
  mkHomTup :: a -> b

instance {-# OVERLAPS #-} MkHomTup 0 [a] (HomTup 0 a) where
  mkHomTup = const $ HomTup @0 @a []

instance
  {-# OVERLAPS #-}
  ( MkHomTup (n - 1) [a] (HomTup (n - 1) a),
    HomTupCons a (HomTup (n - 1) a) (HomTup n a)
  ) =>
  MkHomTup n [a] (HomTup n a)
  where
  mkHomTup (a : as) =
    let asHT = mkHomTup @(n - 1) @[a] @(HomTup (n - 1) a) as
     in homTupCons @a @(HomTup (n - 1) a) @(HomTup n a) a asHT

class HomTupCons a b c where
  homTupCons :: a -> b -> c

instance (m ~ n + 1) => HomTupCons a (HomTup n a) (HomTup m a) where
  homTupCons a (HomTup as) = HomTup (a : as)

homTup ::
  forall n a t u m.
  ( KnownNat n,
    Stream [String] m t,
    MkHomTup n [a] (HomTup n a)
  ) =>
  ParsecT String u m a ->
  ParsecT String u m (HomTup n a)
homTup p = do
  let n = natVal (Proxy @n)
  as <- count @String @m @Char @u (fromIntegral n) p
  return $ mkHomTup @n @[a] @(HomTup n a) as

tuple ::
  forall n a t s u m tok.
  ( KnownNat n,
    MkHomTup n [a] (HomTup n a),
    Stream s m tok,
    ToTup n (HomTup n a) t,
    t ~ ToTupF (HomTup n a)
  ) =>
  ParsecT s u m a ->
  ParsecT s u m (ToTupF (HomTup n a))
tuple =
  fmap (toTup @n @(HomTup n a) @t)
    . fmap (HomTup @n @a)
    . count @s @m @tok @u (fromIntegral $ natVal (Proxy @n))

tuples :: forall n a t s u m. (ToTup n (HomTup n a) t, ToTups [a] [HomTup n a]) => ParsecT s u m [a] -> ParsecT s u m [t]
tuples p = fmap (fmap (toTup @n @(HomTup n a) @t) . toTups @[a] @[HomTup n a]) p

instance (Eq a) => Memberable a (HomTup n a) where
  a ∈ HomTup t = a ∈ t

instance (Eq a) => Memberable a (Solo a) where
  a ∈ MkSolo a' = a == a'

-- instance (Eq a, Memberable a (Solo a)) => Memberable a (a, a) where
--   a ∈ t = let (a', t') = tupSnoc @(a, a) @a @(Solo a) t in a == a' || ((∈) @a @(Solo a) a t')

instance {-# OVERLAPS #-} (Eq a, Memberable a t, f a t ~ TupConsF a t, TupSnoc (f a t) a t, TupSnocF (f a t) ~ (a, t)) => Memberable a (f a t) where
  a ∈ fat = let (a', t) = tupSnoc @(f a t) @a @t fat in a == a' || ((∈) @a @t a t)

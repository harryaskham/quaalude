module Quaalude.Show where

import Data.Tuple.Solo
import Quaalude.Tuple
import GHC.Show

-- | Show instances for tuples
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u, Show v) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u . showString ", " .
      showsPrec 11 v

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u, Show v, Show w) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u . showString ", " .
      showsPrec 11 v . showString ", " .
      showsPrec 11 w

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u, Show v, Show w, Show x) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u . showString ", " .
      showsPrec 11 v . showString ", " .
      showsPrec 11 w . showString ", " .
      showsPrec 11 x

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u, Show v, Show w, Show x, Show y) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u . showString ", " .
      showsPrec 11 v . showString ", " .
      showsPrec 11 w . showString ", " .
      showsPrec 11 x . showString ", " .
      showsPrec 11 y

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r, Show s, Show t, Show u, Show v, Show w, Show x, Show y, Show z) 
  => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  showsPrec d (a, b, c, d', e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) =
    showParen (d > 10) $
      showsPrec 11 a . showString ", " .
      showsPrec 11 b . showString ", " .
      showsPrec 11 c . showString ", " .
      showsPrec 11 d' . showString ", " .
      showsPrec 11 e . showString ", " .
      showsPrec 11 f . showString ", " .
      showsPrec 11 g . showString ", " .
      showsPrec 11 h . showString ", " .
      showsPrec 11 i . showString ", " .
      showsPrec 11 j . showString ", " .
      showsPrec 11 k . showString ", " .
      showsPrec 11 l . showString ", " .
      showsPrec 11 m . showString ", " .
      showsPrec 11 n . showString ", " .
      showsPrec 11 o . showString ", " .
      showsPrec 11 p . showString ", " .
      showsPrec 11 q . showString ", " .
      showsPrec 11 r . showString ", " .
      showsPrec 11 s . showString ", " .
      showsPrec 11 t . showString ", " .
      showsPrec 11 u . showString ", " .
      showsPrec 11 v . showString ", " .
      showsPrec 11 w . showString ", " .
      showsPrec 11 x . showString ", " .
      showsPrec 11 y . showString ", " .
      showsPrec 11 z

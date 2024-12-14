module Quaalude.Tuple where

import Data.HList
import Data.List qualified as L
import GHC.TypeLits
import Quaalude.Collection
import Text.Parsec
import Prelude hiding (natVal)

type family TupConsF a as where
  TupConsF a () = a
  TupConsF a (b, c) = (a, b, c)
  TupConsF a (b, c, d) = (a, b, c, d)
  TupConsF a (b, c, d, e) = (a, b, c, d, e)
  TupConsF a (b, c, d, e, f) = (a, b, c, d, e, f)
  TupConsF a (b, c, d, e, f, g) = (a, b, c, d, e, f, g)
  TupConsF a (b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, h)
  TupConsF a (b, c, d, e, f, g, h, i) = (a, b, c, d, e, f, g, h, i)
  TupConsF a (b, c, d, e, f, g, h, i, j) = (a, b, c, d, e, f, g, h, i, j)
  TupConsF a b = (a, b)

type a × b = TupConsF a b

class TupCons a b c where
  tupCons :: a -> b -> c

instance TupCons a () a where
  tupCons a () = a

instance TupCons a b (a, b) where
  tupCons a b = (a, b)

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

type family TupSnocF a where
  TupSnocF () = TypeError ('Text "TupSnocF: ()")
  TupSnocF (a, b) = (a, b)
  TupSnocF (a, b, c) = (a, (b, c))
  TupSnocF (a, b, c, d) = (a, (b, c, d))
  TupSnocF (a, b, c, d, e) = (a, (b, c, d, e))
  TupSnocF (a, b, c, d, e, f) = (a, (b, c, d, e, f))
  TupSnocF (a, b, c, d, e, f, g) = (a, (b, c, d, e, f, g))
  TupSnocF (a, b, c, d, e, f, g, h) = (a, (b, c, d, e, f, g, h))
  TupSnocF (a, b, c, d, e, f, g, h, i) = (a, (b, c, d, e, f, g, h, i))
  TupSnocF (a, b, c, d, e, f, g, h, i, j) = (a, (b, c, d, e, f, g, h, i, j))
  TupSnocF a = (a, ())

class TupSnoc a head tail where
  tupSnoc :: a -> (head, tail)

instance TupSnoc (a, b) a b where
  tupSnoc (a, b) = (a, b)

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

type family TupFstF a where
  TupFstF (a, b) = a
  TupFstF (a, b, c) = a
  TupFstF (a, b, c, d) = a
  TupFstF (a, b, c, d, e) = a
  TupFstF (a, b, c, d, e, f) = a
  TupFstF (a, b, c, d, e, f, g) = a
  TupFstF (a, b, c, d, e, f, g, h) = a
  TupFstF (a, b, c, d, e, f, g, h, i) = a
  TupFstF (a, b, c, d, e, f, g, h, i, j) = a
  TupFstF () = TypeError ('Text "TupFstF: ()")
  TupFstF a = a

class TupFst a fst where
  tupFst :: a -> fst

instance (TupSnoc a fst tail) => TupFst a fst where
  tupFst = fst . tupSnoc @a @fst @tail

type family TupNthF n a where
  TupNthF 0 a = TupFstF a
  TupNthF n a = TupNthF (n - 1) (TupTailF a)

class TupNth n a nth where
  tupNth :: a -> nth

instance (TupFst a fst) => TupNth 0 a fst where
  tupNth = tupFst

instance (TupTail a tail, TupNth (n - 1) tail nth) => TupNth n a nth where
  tupNth = tupNth @(n - 1) @tail @nth . tupTail @a @tail

type family TupHeadF a where
  TupHeadF () = TypeError ('Text "TupHeadF: ()")
  TupHeadF a = TupFstF a

class TupHead a head where
  tupHead :: a -> head

instance (TupSnoc a head tail) => TupHead a head where
  tupHead = fst . tupSnoc @a @head @tail

type family TupTailF a where
  TupTailF () = TypeError ('Text "TupTailF: ()")
  TupTailF a = TupNthF 1 (TupSnocF a)

class TupTail a tail where
  tupTail :: a -> tail

instance (tail ~ TupTailF a, TupSnoc a head tail) => TupTail a tail where
  tupTail = snd . tupSnoc @a @head @tail

type family TupAppendF a b where
  TupAppendF () b = b
  TupAppendF a b = TupConsF (TupHeadF a) (TupAppendF (TupTailF a) b)

class TupAppend a b c where
  tupAppend :: a -> b -> c

instance TupAppend () b b where
  tupAppend () b = b

instance
  ( TupSnoc a head tail,
    TupAppend tail b rest,
    TupCons head rest res
  ) =>
  TupAppend a b res
  where
  tupAppend a b =
    let (head, tail) = tupSnoc a
     in tupCons @head @rest @res head (tupAppend @tail @b @rest tail b)

type family Tup2ListF a where
  Tup2ListF () = '[]
  Tup2ListF (a, b) = '[a, b]
  Tup2ListF (a, b, c) = '[a, b, c]
  Tup2ListF (a, b, c, d) = '[a, b, c, d]
  Tup2ListF (a, b, c, d, e) = '[a, b, c, d, e]
  Tup2ListF (a, b, c, d, e, f) = '[a, b, c, d, e, f]
  Tup2ListF (a, b, c, d, e, f, g) = '[a, b, c, d, e, f, g]
  Tup2ListF (a, b, c, d, e, f, g, h) = '[a, b, c, d, e, f, g, h]
  Tup2ListF (a, b, c, d, e, f, g, h, i) = '[a, b, c, d, e, f, g, h, i]
  Tup2ListF (a, b, c, d, e, f, g, h, i, j) = '[a, b, c, d, e, f, g, h, i, j]
  Tup2ListF a = '[a]

type family List2TupF a where
  List2TupF '[] = ()
  List2TupF '[a] = a
  List2TupF '[a, b] = (a, b)
  List2TupF '[a, b, c] = (a, b, c)
  List2TupF '[a, b, c, d] = (a, b, c, d)
  List2TupF '[a, b, c, d, e] = (a, b, c, d, e)
  List2TupF '[a, b, c, d, e, f] = (a, b, c, d, e, f)
  List2TupF '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
  List2TupF '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)
  List2TupF '[a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)
  List2TupF '[a, b, c, d, e, f, g, h, i, j] = (a, b, c, d, e, f, g, h, i, j)
  List2TupF e = TypeError ('Text "List2TupF: " ':<>: 'ShowType e)

class Tup2List t h where
  tup2List :: t -> HList h

instance Tup2List () '[] where
  tup2List _ = HNil

instance
  ( TupSnoc t head tail,
    Tup2List tail ltail
  ) =>
  Tup2List t (head ': ltail)
  where
  tup2List t =
    let (head, tail) = tupSnoc @t @head @tail t
     in head .*. tup2List @tail @ltail tail

class List2Tup l t where
  list2Tup :: HList l -> t

instance List2Tup '[] () where
  list2Tup = const ()

instance
  ( List2Tup ltail tail,
    TupCons head tail t
  ) =>
  List2Tup (head ': ltail) t
  where
  list2Tup (HCons head htail) = tupCons @head @tail @t head (list2Tup @ltail @tail htail)

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

newtype HomTup (l :: Nat) a = HomTup [a] deriving (Show)

type family ToTupF a where
  ToTupF (HomTup 0 _) = ()
  ToTupF (HomTup n a) = TupConsF a (ToTupF (HomTup (n - 1) a))
  ToTupF [a] = [ToTupF a]
  ToTupF a = a

class ToTup a b where
  toTup :: a -> b

instance {-# OVERLAPS #-} (ToTups a b, ToTup b c) => ToTup a c where
  toTup = toTup @b @c . toTups @a @b

instance {-# OVERLAPS #-} (ToTup a b) => ToTup [a] [b] where
  toTup = fmap toTup

instance {-# OVERLAPS #-} ToTup (HomTup 0 a) () where
  toTup _ = ()

instance
  {-# OVERLAPPING #-}
  ( ToTup (HomTup (n - 1) a) (ToTupF (HomTup (n - 1) a)),
    TupCons a (ToTupF (HomTup (n - 1) a)) t,
    t ~ ToTupF (HomTup n a)
  ) =>
  ToTup (HomTup n a) t
  where
  toTup (HomTup (a : as)) =
    tupCons @a @(ToTupF (HomTup (n - 1) a)) @(ToTupF (HomTup n a))
      a
      (toTup @(HomTup (n - 1) a) @(ToTupF (HomTup (n - 1) a)) (HomTup @(n - 1) @a as))

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

instance (List2Tup l t) => ToTups (HList l) t where
  toTups = list2Tup @l @t

class MkHomTup n a b where
  mkHomTup :: a -> b

instance {-# OVERLAPPING #-} MkHomTup 0 [a] (HomTup 0 a) where
  mkHomTup = const $ HomTup @0 @a []

instance
  {-# OVERLAPPABLE #-}
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
  forall n a s u m t.
  ( Stream s m t,
    KnownNat n,
    MkHomTup n [a] (HomTup n a)
  ) =>
  ParsecT s u m a ->
  ParsecT s u m (HomTup n a)
homTup p = do
  let n = natVal (Proxy @n)
  as <- count @s @m @t @u (fromIntegral n) p
  return $ mkHomTup @n @[a] @(HomTup n a) as

tuples :: forall n b a s u m. (ToTup [HomTup n a] b, ToTups [a] [HomTup n a]) => ParsecT s u m [a] -> ParsecT s u m b
tuples p = fmap (toTup @[HomTup n a] @b . toTups @[a] @[HomTup n a]) p

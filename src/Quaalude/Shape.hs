module Quaalude.Shape where

import Data.Array
import Data.MonoTraversable
import Data.Sequence hiding (take, (><))
import Quaalude.Alias
import Quaalude.BoundedSet
import Quaalude.Collection
import Quaalude.Coord
import Quaalude.Geometry
import Quaalude.Grid
import Quaalude.LossQ
import Quaalude.Math
import Quaalude.Tracers
import Quaalude.Tuple
import Quaalude.Unary
import Quaalude.Util
import Prelude hiding (filter, take)

data Shape a where
  EmptyShape :: Shape a
  Invalid :: Shape a
  Shape :: (Ord a, MkDiffList a) => Seq a -> Set a -> DiffList a -> (a, a) -> Shape a

deriving instance (Show a) => Show (Shape a)

instance (Eq (i, i), Integral i) => Eq (Shape (i, i)) where
  (==) EmptyShape = \case
    EmptyShape -> True
    _ -> False
  (==) a@(Shape cs _ _ _) = \case
    b@(Shape cs' _ _ _) -> cs ≡ cs'
    _ -> False
  (==) _ = const False

instance (Ord a, Eq (Shape a)) => Ord (Shape a) where
  compare a@(Shape cs _ _ _) b@(Shape cs' _ _ _)
    | a ≡ b = EQ
    | otherwise = compare cs cs'
  compare EmptyShape EmptyShape = EQ
  compare EmptyShape (Shape _ _ _ _) = LT
  compare (Shape _ _ _ _) EmptyShape = GT
  compare _ _ = LT

instance Sizable (Shape a) where
  size Invalid = 0
  size EmptyShape = 0
  size (Shape cs _ _ _) = size cs

instance (ShapeLike Shape i, Num i, Ord i) => Rotatable (Shape (i, i)) where
  (↺) EmptyShape = EmptyShape
  (↺) Invalid = Invalid
  (↺) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ↺)

  (↻) EmptyShape = EmptyShape
  (↻) Invalid = Invalid
  (↻) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ↻)

instance (ShapeLike Shape i, Num i, Ord i) => HMirrorable (Shape (i, i)) where
  (◐) EmptyShape = EmptyShape
  (◐) Invalid = Invalid
  (◐) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ◐)

instance (ShapeLike Shape i, Num i, Ord i) => VMirrorable (Shape (i, i)) where
  (◓) EmptyShape = EmptyShape
  (◓) Invalid = Invalid
  (◓) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ◓)

instance (ShapeLike Shape i, Num i, Ord i) => Transposable (Shape (i, i)) where
  (⊤) EmptyShape = EmptyShape
  (⊤) Invalid = Invalid
  (⊤) (Shape cs s ds bs) = mkShape ((mk @BoundedSet (un cs)) ⊤)

instance (Num i, Show i, Integral i, Coord' i i (i, i)) => Originable Shape (i, i) where
  origin = (0, 0)
  toOrigin Invalid = Invalid
  toOrigin EmptyShape = EmptyShape
  toOrigin shape@(Shape _ _ _ ((lx, ly), (ux, uy))) =
    offsetShape (negate lx, negate ly) shape

instance (Originable Shape a) => Originable LossShape a where
  origin = origin @Shape @a
  toOrigin (LossShape s) = LossShape (toOrigin s)

instance Foldable Shape where
  foldr _ accum Invalid = accum
  foldr _ accum EmptyShape = accum
  foldr f accum (Shape _ s _ _) = foldr f accum s

instance Foldable LossShape where
  foldr f accum (LossShape s) = foldr f accum s

instance (ShapeLike Shape i) => Filterable Shape (i, i) where
  (Shape cs _ _ _) |-?-> f = mk $ un cs |-?-> f
  s |-?-> _ = s

instance (Filterable Shape (i, i)) => Filterable LossShape (i, i) where
  (LossShape s) |-?-> f = LossShape (s |-?-> f)

instance (Semigroup (Shape a)) => Monoid (Shape a) where
  mempty = EmptyShape

instance (Bimaximum a, Biminimum a, MkDiffList a) => Semigroup (Shape a) where
  Invalid <> s = Invalid
  s <> Invalid = Invalid
  EmptyShape <> s = s
  s <> EmptyShape = s
  (Shape cs0@(_ :|> l0) s0 ds0 (lb0, ub0)) <> (Shape cs1@(h1 :<| _) s1 ds1 (lb1, ub1)) =
    let s01 = s0 ∪ s1
     in ((s01 |.|) ≡ (s0 |.|) + (s1 |.|))
          ??? (Shape (cs0 >< cs1) s01 (diffListConcatVia ds0 l0 h1 ds1) (biminimum [lb0, lb1], bimaximum [ub0, ub1]))
          $ Invalid

type ShapeLikeC s i =
  ( Show i,
    Integral i,
    Coord' i i (i, i),
    Semigroup (s (i, i)),
    Monoid (s (i, i))
  ) ::
    Constraint

instance (ShapeLike LossShape i) => IntLoss (LossShape (i, i)) where
  loss (LossShape (Shape cs s ds bs)) = negate (size s)

instance (ShapeLike Shape i) => Mkable Shape (i, i) where
  mk = mkShape @Shape @i

instance (ShapeLike LossShape i) => Mkable LossShape (i, i) where
  mk = mkShape @LossShape @i

instance Unable Shape where
  un (Shape _ s _ _) = un s

instance Unable LossShape where
  un (LossShape s) = un s

instance (ShapeLike Shape i) => Differenceable Shape (i, i) where
  Invalid ∖ _ = Invalid
  _ ∖ Invalid = Invalid
  s ∖ EmptyShape = s
  EmptyShape ∖ _ = EmptyShape
  (Shape cs s ds bs) ∖ (Shape cs' s' ds' bs') = mkShape (s ∖ s')

instance (Differenceable Shape (i, i)) => Differenceable LossShape (i, i) where
  (LossShape s) ∖ (LossShape s') = LossShape (s ∖ s')

instance Memberable (i, i) (Shape (i, i)) where
  a ∈ (Shape _ s _ _) = a ∈ s
  a ∈ _ = False

instance Memberable (i, i) (LossShape (i, i)) where
  a ∈ (LossShape (Shape _ s _ _)) = a ∈ s
  a ∈ _ = False

instance (ShapeLike Shape i) => Unionable (Shape (i, i)) where
  Invalid ∪ _ = Invalid
  _ ∪ Invalid = Invalid
  s ∪ EmptyShape = s
  EmptyShape ∪ s = s
  (Shape _ s _ _) ∪ (Shape _ s' _ _) = mk ∘ un $ s ∪ s'

instance (Unionable (Shape (i, i))) => Unionable (LossShape (i, i)) where
  (LossShape s) ∪ (LossShape s') = LossShape (s ∪ s')

instance (ShapeLike Shape i) => Intersectable (Shape (i, i)) where
  Invalid ∩ _ = Invalid
  _ ∩ Invalid = Invalid
  s ∩ EmptyShape = EmptyShape
  EmptyShape ∩ s = EmptyShape
  (Shape _ s _ _) ∩ (Shape _ s' _ _) = mk ∘ un $ s ∩ s'

instance (Intersectable (Shape (i, i))) => Intersectable (LossShape (i, i)) where
  (LossShape s) ∩ (LossShape s') = LossShape (s ∩ s')

class ShapeLike s i where
  mkShape :: (Foldable m, Unable m) => m (i, i) -> s (i, i)
  validShape :: (i, i) -> s (i, i) -> 𝔹
  boundedShape :: (i, i) -> s (i, i) -> 𝔹
  shapeWH :: s (i, i) -> (i, i)
  offsetShape :: (i, i) -> s (i, i) -> s (i, i)

  (⇆⇅) :: s (i, i) -> (i, i) -> s (i, i)
  s ⇆⇅ (x, y) = offsetShape (x, y) s

  (⇆) :: s (i, i) -> i -> s (i, i)
  default (⇆) :: (Num i) => s (i, i) -> i -> s (i, i)
  s ⇆ x = offsetShape (x, 0) s

  (⇅) :: s (i, i) -> i -> s (i, i)
  default (⇅) :: (Num i) => s (i, i) -> i -> s (i, i)
  s ⇅ y = offsetShape (0, y) s

  area :: (a ~ (i, i)) => s (i, i) -> i
  contiguous :: s (i, i) -> 𝔹

instance (ShapeLikeC Set i) => ShapeLike Set i where
  mkShape = foldMap mk₁
  validShape = boundedShape
  boundedShape (w, h) s = setMap (\(x, y) -> x ≤ w ∧ y ≤ h) s ≡ mk₁ True
  shapeWH s
    | s ≡ (∅) = (0, 0)
    | otherwise =
        let (minX, minY) = biminimum s
            (maxX, maxY) = bimaximum s
         in (maxX - minX + 1, maxY - minY + 1)
  area s = let (w, h) = shapeWH s in w ⋅ h
  offsetShape (x, y) = setMap (bimap (+ x) (+ y))
  contiguous s = go s (mkSeq (take 1 $ un s))
    where
      go left (c :<| q)
        | c ∉ s ∨ c ∉ left = go left q
        | otherwise = go (left ∸ c) (q >< mk (neighborsNoDiags c))
      go left _ = left ≡ (∅)

toG :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => s (i, i) -> ".#X" ▦ (i, i)
toG s
  | s ≡ (∅) = mkGrid []
  | otherwise =
      let (minX, minY) = biminimum s
          (maxX, maxY) = bimaximum s
       in mkGrid [((x, y), (x, y) ∈ s ??? (#"#" □) $ (#"." □)) | x <- [0 .. maxX + 3], y <- [0 .. maxY + 3]]

showShape :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => s (i, i) -> Text
showShape s
  | s ≡ (∅) = "∅"
  | otherwise =
      unlines
        [ tshow (size s),
          pretty (toG s)
        ]

showShapes :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => [s (i, i)] -> Text
showShapes = unlines ∘ fmap showShape

showShapess :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => [[s (i, i)]] -> Text
showShapess = unlines ∘ fmap showShapes

traceShape :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => s (i, i) -> b -> b
traceShape s a = traceTextLn (showShape s) a

traceShapeId :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => s (i, i) -> s (i, i)
traceShapeId s = traceTextLn (showShape s) s

traceShapeLabelled :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => (Show t) => t -> s (i, i) -> a -> a
traceShapeLabelled l s a = traceTextLn (unlines [tshow l, (showShape s)]) a

traceShapeIdLabelled :: (Integral i, Eq (s (i, i)), Memberable (i, i) (s (i, i)), Foldable s, Monoid (s (i, i)), Sizable (s (i, i)), Coord' i i (i, i), ShapeLike s i) => (Show t) => t -> s (i, i) -> s (i, i)
traceShapeIdLabelled l s = traceTextLn (unlines [tshow l, (showShape s)]) s

instance (ShapeLikeC BoundedSet i) => ShapeLike BoundedSet i where
  mkShape = mk ∘ un
  validShape = boundedShape
  boundedShape (w, h) (BoundedSet (minX, minY) (maxX, maxY) _) = minX ≥ 0 ∧ minY ≥ 0 ∧ maxX < w ∧ maxY < h
  shapeWH (BoundedSet (minX, minY) (maxX, maxY) _) = (maxX - minX + 1, maxY - minY + 1)
  area s = let (w, h) = shapeWH s in w ⋅ h
  offsetShape (x, y) bs@(BoundedSet _ _ s)
    | s ≡ (∅) = bs
    | otherwise = omap (bimap (+ x) (+ y)) bs
  contiguous (BoundedSet _ _ s) = go s (mkSeq (take 1 $ un s))
    where
      go left (c :<| q)
        | c ∉ s ∨ c ∉ left = go left q
        | otherwise = go (left ∸ c) (q >< mk (neighborsNoDiags c))
      go left _ = left ≡ (∅)

instance (ShapeLikeC Shape i) => ShapeLike Shape i where
  mkShape cs = case toList cs of
    [] -> EmptyShape
    cs ->
      let s = mkSet cs
          lb = biminimum cs
          ub = bimaximum cs
       in if s |≢| cs
            then Invalid
            else Shape (mk cs) s (mkDiffList cs) (lb, ub)

  validShape _ Invalid = False
  validShape _ EmptyShape = False
  validShape wh shape = boundedShape wh shape

  boundedShape _ Invalid = False
  boundedShape (w, h) shape = let (sw, sh) = shapeWH shape in sw ≤ w ∧ sh ≤ h

  shapeWH Invalid = (0, 0)
  shapeWH EmptyShape = (0, 0)
  shapeWH (Shape _ _ _ ((lx, ly), (ux, uy))) = (ux - lx + 1, uy - ly + 1)

  offsetShape (x, y) Invalid = Invalid
  offsetShape (x, y) EmptyShape = EmptyShape
  offsetShape (x, y) (Shape cs s ds ((lx, ly), (ux, uy))) =
    let cs' = bimap (+ x) (+ y) <$> cs
     in Shape cs' (mk $ un cs') ds ((lx + x, ly + y), (ux + x, uy + y))

  area shape = (*) $@ shapeWH shape

  contiguous Invalid = False
  contiguous EmptyShape = True
  contiguous (Shape cs'@(c :<| _) _ _ _) = go cs (mkSeq [c])
    where
      cs = mkSet (un cs')
      go left (c :<| q)
        | c ∉ cs ∨ c ∉ left = go left q
        | otherwise = go (left ∸ c) (q >< mk (neighborsNoDiags c))
      go left _ = left ≡ (∅)

data LossShape a = LossShape {unLossShape :: Shape a} deriving (Show)

instance (Eq (Shape a)) => Eq (LossShape a) where
  (LossShape a) == (LossShape b) = a == b

type instance LossF (LossShape a) = LossShape a

type instance LossF (Integer, (Integer, Integer)) = (Integer, (Integer, Integer))

instance (Show i, Integral i, ShapeLike Shape i, Coord' i i (i, i)) => ShapeLike LossShape i where
  mkShape cs = LossShape (mkShape @Shape @i cs)
  validShape wh (LossShape s) = validShape wh s
  boundedShape wh (LossShape s) = boundedShape wh s
  shapeWH (LossShape s) = shapeWH s
  offsetShape o (LossShape s) = LossShape (offsetShape o s)
  area (LossShape s) = area s
  contiguous (LossShape s) = contiguous s

instance (Ord i, Integral i, Show i, Coord' i i (i, i)) => Ord (LossShape (i, i)) where
  compare =
    let loss shape = negate (size shape)
     in comparing loss

instance Sizable (LossShape a) where
  size (LossShape s) = size s

instance (Rotatable (Shape a)) => Rotatable (LossShape a) where
  (↺) (LossShape s) = LossShape $ (↺) s
  (↻) (LossShape s) = LossShape $ (↻) s

instance (HMirrorable (Shape a)) => HMirrorable (LossShape a) where
  (◐) (LossShape s) = LossShape $ (◐) s

instance (VMirrorable (Shape a)) => VMirrorable (LossShape a) where
  (◓) (LossShape s) = LossShape $ (◓) s

instance (Transposable (Shape (i, i))) => Transposable (LossShape (i, i)) where
  (⊤) (LossShape s) = LossShape (s ⊤)

instance Magnitude (Shape a) where
  (|.|) Invalid = 0
  (|.|) EmptyShape = 0
  (|.|) (Shape cs _ _ _) = (cs |.|)

type instance MagnitudeF (Shape a) = Integer

type instance MagnitudeF (LossShape a) = Integer

instance Magnitude (LossShape a) where
  (|.|) (LossShape s) = (s |.|)

instance (Semigroup (Shape a)) => Semigroup (LossShape a) where
  (LossShape a) <> (LossShape b) = LossShape (a <> b)

instance (Monoid (Shape a)) => Monoid (LossShape a) where
  mempty = LossShape mempty

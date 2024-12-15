module Quaalude.Coord where

import Data.Array
import Data.Default
import Data.Foldable qualified as F
import Quaalude.Collection
import Quaalude.Tuple
import Text.Megaparsec (count')
import Text.Show qualified as TS

data Dir2 = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Ord, Enum, Bounded)

type Dir² = Dir2

instance TS.Show Dir2 where
  show DirUp = "↑"
  show DirDown = "↓"
  show DirLeft = "←"
  show DirRight = "→"

instance Default Dir2 where
  def = DirUp

instance Ix Dir2 where
  range (a, b) = [a .. b]
  index (a, _) c = fromEnum c - fromEnum a
  inRange (a, b) c = c >= a && c <= b

data Dir3 = D3xP | D3xN | D3yP | D3yN | D3zP | D3zN deriving (Show, Eq, Ord, Enum, Bounded)

instance Default Dir3 where
  def = D3xP

udlrToDir2 :: Char -> Dir2
udlrToDir2 'u' = DirUp
udlrToDir2 'd' = DirDown
udlrToDir2 'l' = DirLeft
udlrToDir2 'r' = DirRight
udlrToDir2 'U' = DirUp
udlrToDir2 'D' = DirDown
udlrToDir2 'L' = DirLeft
udlrToDir2 'R' = DirRight
udlrToDir2 c = error $ "Invalid udlr: " <> show c

nsewToDir2 :: Char -> Dir2
nsewToDir2 'n' = DirUp
nsewToDir2 's' = DirDown
nsewToDir2 'e' = DirRight
nsewToDir2 'w' = DirLeft
nsewToDir2 'N' = DirUp
nsewToDir2 'S' = DirDown
nsewToDir2 'E' = DirRight
nsewToDir2 'W' = DirLeft
nsewToDir2 c = error $ "Invalid nsew: " <> show c

fromArrow2 :: Char -> Dir2
fromArrow2 '^' = DirUp
fromArrow2 'v' = DirDown
fromArrow2 '>' = DirRight
fromArrow2 '<' = DirLeft
fromArrow2 c = error $ "Invalid arrow: " <> show c

type Coord2' a = (a, a)

type Coord2 = Coord2' Int

type Coord3 = (Int, Int, Int)

type Coord4 = (Int, Int, Int, Int)

type family Idem2 a where
  Idem2 (a, a) = (a, a)
  Idem2 Integer = (Integer, Integer)
  Idem2 Int = (Int, Int)
  Idem2 Rational = (Rational, Rational)

class Coord' i a k where
  fromXY :: (i, i) -> k
  toXY :: k -> (i, i)
  mapXY :: ((i, i) -> (i, i)) -> k -> k
  default mapXY :: (k ~ (a, a)) => ((i, i) -> (i, i)) -> k -> k
  mapXY f (a, b) = fromXY @i @a (f (toXY @i @a (a, b)))

instance (Coord' i a (a, a)) => Coord' i (a, a) (a, a) where
  fromXY = fromXY @i @a @(a, a)
  toXY = toXY @i @a @(a, a)
  mapXY = mapXY @i @a @(a, a)

instance (Num a, Integral a, Coord' a a (a, a)) => Coord' Int a (a, a) where
  fromXY (x, y) = (fromInteger $ fromIntegral x, fromInteger $ fromIntegral y)
  toXY (a, b) = (round $ fromIntegral a, round $ fromIntegral b)

instance Coord' Integer Integer (Integer, Integer) where
  fromXY (x, y) = (fromIntegral x, fromIntegral y)
  toXY (a, b) = (fromIntegral a, fromIntegral b)

-- instance Coord' Int Int (Int, Int) where
--   fromXY (x, y) = (fromIntegral x, fromIntegral y)
--   toXY (a, b) = (fromIntegral a, fromIntegral b)

instance Coord' Rational Rational (Rational, Rational) where
  fromXY (x, y) = (fromRational x, fromRational y)
  toXY (a, b) = (toRational a, toRational b)

class
  ( Coord' Int a a
  ) =>
  Coord a

instance
  ( Coord' Int a a
  ) =>
  Coord a

manhattan0 :: Coord2 -> Int
manhattan0 = (+) <$> (abs . fst) <*> (abs . snd)

manhattan :: forall a. (Coord (a, a), Num a) => (a, a) -> (a, a) -> a
manhattan a b =
  let (x1, y1) = toXY @Int @(a, a) a
      (x2, y2) = toXY @Int @(a, a) b
   in fst $ fromXY @Int @(a, a) @(a, a) (abs (x1 - x2) + abs (y1 - y2), 0 :: Int)

manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

manhattan4 :: Coord4 -> Coord4 -> Int
manhattan4 (x1, y1, z1, w1) (x2, y2, z2, w2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2) + abs (w1 - w2)

move :: forall i c. (Num c, Num i, Coord' i c (c, c)) => Dir2 -> i -> (c, c) -> (c, c)
move DirUp n = mapXY @i @(c, c) (second (subtract n))
move DirDown n = mapXY @i @(c, c) (second (+ n))
move DirLeft n = mapXY @i @(c, c) (first (subtract n))
move DirRight n = mapXY @i @(c, c) (first (+ n))

move3 :: Dir3 -> Int -> Coord3 -> Coord3
move3 D3xP n (x, y, z) = (x + n, y, z)
move3 D3xN n (x, y, z) = (x - n, y, z)
move3 D3yP n (x, y, z) = (x, y + n, z)
move3 D3yN n (x, y, z) = (x, y - n, z)
move3 D3zP n (x, y, z) = (x, y, z + n)
move3 D3zN n (x, y, z) = (x, y, z - n)

turnCW :: Dir2 -> Dir2
turnCW DirUp = DirRight
turnCW DirRight = DirDown
turnCW DirDown = DirLeft
turnCW DirLeft = DirUp

turn180 :: Dir2 -> Dir2
turn180 = turnCW . turnCW

opposite :: Dir2 -> Dir2
opposite = turn180

turnCCW :: Dir2 -> Dir2
turnCCW = turnCW . turnCW . turnCW

rlToTurn :: Char -> (Dir2 -> Dir2)
rlToTurn 'r' = turnCW
rlToTurn 'R' = turnCW
rlToTurn 'l' = turnCCW
rlToTurn 'L' = turnCCW
rlToTurn c = error $ "Invalid rl: " <> show c

class Neighbors (n :: Nat) f a g where
  neighs :: a -> g -> f a

class Vicinity (n :: Nat) f a where
  vicinity' :: (Integral i) => a -> i -> f a
  vicinity :: a -> f a
  default vicinity :: a -> f a
  vicinity a = vicinity' @n @f @a a 1

instance (Num a, Integral a, Monad f, Alternative f, Mkable f (a, a), Memberable (a, a) g) => Neighbors 4 f (a, a) g where
  neighs c g = [n | n <- mk (neighborsNoDiags c), n ∈ g]

instance (Num a, Integral a, Monad f, Alternative f, Mkable f (a, a)) => Vicinity 4 f (a, a) where
  vicinity' c i =
    let bounds = [-i .. i]
        (ns :: [(a, a)]) = [c + (fromIntegral xo, fromIntegral yo) | xo <- bounds, yo <- bounds, (xo, yo) /= (0, 0), xo == 0 || yo == 0]
     in mk ns

instance (Num a, Integral a, Monad f, Alternative f, Mkable f (a, a), Memberable (a, a) g) => Neighbors 8 f (a, a) g where
  neighs c g = [n | n <- mk (neighbors c), n ∈ g]

instance (Num a, Integral a, Monad f, Alternative f, Mkable f (a, a)) => Vicinity 8 f (a, a) where
  vicinity' c i =
    let bounds = [-i .. i]
        (ns :: [(a, a)]) = [c + (fromIntegral xo, fromIntegral yo) | xo <- bounds, yo <- bounds, (xo, yo) /= (0, 0)]
     in mk ns

neighbors :: (Num a) => (a, a) -> [(a, a)]
neighbors (x, y) =
  [ (x + fromIntegral xO, y + fromIntegral yO)
    | xO <- [-1 .. 1],
      yO <- [-1 .. 1],
      xO /= 0 || yO /= 0
  ]

neighborsNoDiags :: (Num a) => (a, a) -> [(a, a)]
neighborsNoDiags (x, y) =
  [ (x + 1, y),
    (x - 1, y),
    (x, y + 1),
    (x, y - 1)
  ]

neighbors3 :: Coord3 -> [Coord3]
neighbors3 (x, y, z) =
  [ (x + xO, y + yO, z + zO)
    | xO <- [-1 .. 1],
      yO <- [-1 .. 1],
      zO <- [-1 .. 1],
      xO /= 0 || yO /= 0 || zO /= 0
  ]

partitionSpace :: Coord3 -> Coord3 -> [(Coord3, Coord3)]
partitionSpace (lx, ly, lz) (ux, uy, uz) =
  [ ((lx, ly, lz), (hx, hy, hz)),
    ((hx, ly, lz), (ux, hy, hz)),
    ((lx, hy, lz), (hx, uy, hz)),
    ((hx, hy, lz), (ux, uy, hz)),
    ((lx, ly, hz), (hx, hy, uz)),
    ((hx, ly, hz), (ux, hy, uz)),
    ((lx, hy, hz), (hx, uy, uz)),
    ((hx, hy, hz), (ux, uy, uz))
  ]
  where
    hx = (lx + ux) `div` 2
    hy = (ly + uy) `div` 2
    hz = (lz + uz) `div` 2

-- Gets the point-set between two lines. Assumes lines are provided in sorted order.
linePoints :: (Coord2, Coord2) -> [Coord2]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]
  | otherwise = [(x, y) | x <- [x1 .. x2], let y = y1 + (x - x1) * signum (y2 - y1)]

wrap :: Int -> Int -> Coord2 -> Coord2
wrap w h (x, y) = (x `mod` w, y `mod` h)

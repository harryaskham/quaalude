{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Quaalude.Grid where

import Control.Monad
import Control.Monad.Memo.Vector (Vector)
import Control.Monad.ST (ST, runST)
import Data.Array (assocs)
import Data.Array qualified as A
import Data.Array.IO (IOArray, getBounds, readArray)
import Data.Array.MArray (MArray, getAssocs, newArray, newArray_, writeArray)
import Data.Array.ST qualified as STA
import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.Default
import Data.Fin (Fin)
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.List (groupBy)
import Data.List.Extra (groupOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as STV
import Quaalude.Alias
import Quaalude.Collection
import Quaalude.Coord
import Quaalude.Tracers
import Quaalude.Unary
import Quaalude.Util (both, bothM, unjust, (.<.), (<$$>))
import Relude.Unsafe qualified as U
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (filter)

emptyGrid :: (Griddable Identity g k a) => g k a
emptyGrid = runIdentity emptyGridM

mkGrid :: (Griddable Identity g k a) => [(k, a)] -> g k a
mkGrid = runIdentity . mkGridM

unGrid :: (Griddable Identity g k a) => g k a -> [(k, a)]
unGrid = runIdentity . unGridM

coords :: (Griddable Identity g k a) => g k a -> [k]
coords = runIdentity . coordsM

cells :: (Griddable Identity g k a) => g k a -> [a]
cells = runIdentity . cellsM

gridGetMaybe :: (Griddable Identity g k a) => k -> g k a -> Maybe a
gridGetMaybe c g = runIdentity $ gridGetMaybeM c g

(||?) :: (Griddable Identity g k a) => g k a -> k -> Maybe a
g ||? c = gridGetMaybe c g

gridGet :: (Griddable Identity g k a) => k -> g k a -> a
gridGet c g = runIdentity $ gridGetM c g

(||!) :: (Griddable Identity g k a) => g k a -> k -> a
g ||! c = gridGet c g

gridSet :: (Griddable Identity g k a) => a -> k -> g k a -> g k a
gridSet a c g = runIdentity $ gridSetM a c g

(||.) :: (Griddable Identity g k a) => g k a -> (k, a) -> g k a
g ||. (c, a) = gridSet a c g

gridModify :: (Griddable Identity g k a) => (a -> a) -> k -> g k a -> g k a
gridModify f c g = runIdentity $ gridModifyM f c g

(||~) :: (Griddable Identity g k a) => g k a -> (k, a -> a) -> g k a
g ||~ (c, f) = gridModify f c g

maxXY :: (Griddable Identity g k a, Coord k) => g k a -> k
maxXY = runIdentity . maxXYM

minXY :: (Griddable Identity g k a, Coord k) => g k a -> k
minXY = runIdentity . minXYM

gridDims :: (Griddable Identity g k a, Coord k) => g k a -> k
gridDims = runIdentity . gridDimsM

gridFind :: (Griddable Identity g k a) => a -> g k a -> [k]
gridFind a g = runIdentity $ gridFindM a g

gridFindOne :: (Griddable Identity g k a) => a -> g k a -> Maybe k
gridFindOne a g = runIdentity $ gridFindOneM a g

fromCoords :: (Foldable f, Bounded a, Griddable Identity g k a) => a -> f k -> g k a
fromCoords def cs = runIdentity $ fromCoordsM def cs

fillDef :: (Griddable Identity g k a) => a -> g k a -> g k a
fillDef def g = runIdentity $ fillDefM def g

fillEmpty :: (Bounded a, Griddable Identity g k a) => g k a -> g k a
fillEmpty = runIdentity . fillEmptyM

mapCoords :: (Griddable Identity g k a) => (k -> k) -> g k a -> g k a
mapCoords f g = runIdentity $ mapCoordsM f g

filterCoords :: (Griddable Identity g k a) => (k -> Bool) -> g k a -> g k a
filterCoords f g = runIdentity $ filterCoordsM f g

partitionCoords :: (Griddable Identity g k a) => (k -> Bool) -> g k a -> (g k a, g k a)
partitionCoords f g = runIdentity $ partitionCoordsM f g

gridMember :: (Griddable Identity g k a) => k -> g k a -> Bool
gridMember c g = runIdentity $ gridMemberM c g

class (Monad m, Coord k, GridCell a) => Griddable m g k a where
  emptyGridM :: m (g k a)
  emptyGridM = mkGridM []
  mkGridM :: [(k, a)] -> m (g k a)
  unGridM :: g k a -> m [(k, a)]
  coordsM :: g k a -> m [k]
  coordsM g = fst <$$> unGridM g
  cellsM :: g k a -> m [a]
  cellsM g = snd <$$> unGridM g
  gridGetMaybeM :: k -> g k a -> m (Maybe a)
  (<||?>) :: g k a -> k -> m (Maybe a)
  (<||?>) = flip gridGetMaybeM
  gridGetM :: k -> g k a -> m a
  (<||!>) :: g k a -> k -> m a
  (<||!>) = flip gridGetM
  gridSetM :: a -> k -> g k a -> m (g k a)
  (<||.>) :: g k a -> (k, a) -> m (g k a)
  g <||.> (c, a) = gridSetM a c g
  gridModifyM :: (a -> a) -> k -> g k a -> m (g k a)
  (<||~>) :: g k a -> (k, a -> a) -> m (g k a)
  g <||~> (c, f) = gridModifyM f c g
  maxXYM :: g k a -> m k
  minXYM :: g k a -> m k
  gridDimsM :: g k a -> m k
  gridDimsM g = do
    (x0, y0) <- toXY <$> minXYM g
    (x1, y1) <- toXY <$> maxXYM g
    return $ fromXY (x1 - x0 + 1, y1 - y0 + 1)
  gridFindM :: a -> g k a -> m [k]
  gridFindM a g = (\cs -> [k | (k, v) <- cs, v == a]) <$> unGridM g
  gridFindOneM :: a -> g k a -> m (Maybe k)
  gridFindOneM a g = do
    cs <- gridFindM a g
    case cs of
      [] -> return Nothing
      (c : _) -> return $ Just c
  fromCoordsM :: (Foldable f, Bounded a) => a -> f k -> m (g k a)
  fromCoordsM def cs = do
    e <- emptyGridM
    e' <- foldlM (\g c -> g <||.> (c, def)) e cs
    fillDefM def e'
  fillDefM :: a -> g k a -> m (g k a)
  fillDefM def g = do
    cs <- coordsM g
    elems <- sequence [(c,) . fromMaybe def <$> (g <||?> c) | c <- cs]
    mkGridM elems
  fillEmptyM :: (Bounded a) => g k a -> m (g k a)
  fillEmptyM = fillDefM minBound
  mapCoordsM :: (k -> k) -> g k a -> m (g k a)
  mapCoordsM f g = mkGridM =<< (first f <$$> unGridM g)
  filterCoordsM :: (k -> Bool) -> g k a -> m (g k a)
  filterCoordsM f g = mkGridM . filter (f . fst) =<< unGridM g
  partitionCoordsM :: (k -> Bool) -> g k a -> m (g k a, g k a)
  partitionCoordsM f g = do
    ab <- groupOn (f . fst) <$> unGridM g
    let [a, b] = ab
    gA <- mkGridM a
    gB <- mkGridM b
    return (gA, gB)
  gridMemberM :: k -> g k a -> m Bool
  gridMemberM c g = isJust <$> gridGetMaybeM c g
  (<||∈>) :: k -> g k a -> m Bool
  a <||∈> g = gridMemberM a g
  (<||∉>) :: k -> g k a -> m Bool
  a <||∉> g = not <$> (a <||∈> g)

newtype Grid' k a = Grid (Map k a) deriving (Eq, Ord, Show)

type Grid = Grid' Coord2

type ℤ² = Grid' (ℤ, ℤ)

type ℕ₁₀² = Grid' (ℕ₁₀, ℕ₁₀)

instance (GridCell a, Coord k, Ord k) => Griddable Identity Grid' k a where
  mkGridM = pure . Grid . M.fromList
  unGridM (Grid g) = pure $ M.toList g
  gridGetMaybeM c (Grid g) = pure $ M.lookup c g
  gridGetM c (Grid g) = pure $ g M.! c
  gridSetM a c (Grid g) = pure . Grid $ M.insert c a g
  gridModifyM f c (Grid g) = pure . Grid $ M.adjust f c g
  maxXYM (Grid g) =
    pure $
      fromXY
        ( maximum $ fst . toXY <$> M.keys g,
          maximum $ snd . toXY <$> M.keys g
        )
  minXYM (Grid g) =
    pure $
      fromXY
        ( minimum $ fst . toXY <$> M.keys g,
          minimum $ snd . toXY <$> M.keys g
        )
  mapCoordsM f (Grid g) = pure . Grid $ M.mapKeys f g
  filterCoordsM f (Grid g) = pure . Grid $ M.filterWithKey (\k _ -> f k) g
  partitionCoordsM f (Grid g) = pure . both Grid $ M.partitionWithKey (\k _ -> f k) g
  gridMemberM c (Grid g) = pure $ M.member c g

newtype IntMapGrid' k a = IntMapGrid (IntMap (IntMap a)) deriving (Eq, Ord, Show)

type IntMapGrid = IntMapGrid' Coord2

instance (GridCell a) => Griddable Identity IntMapGrid' Coord2 a where
  mkGridM =
    pure
      . IntMapGrid
      . IM.fromList
      . fmap (\g -> (snd . fst $ uhead g, IM.fromList [(x, c) | ((x, _), c) <- g]))
      . groupOn (snd . fst)
      . sortOn (snd . fst)
  unGridM (IntMapGrid g) = pure $ [((x, y), c) | (y, row) <- IM.toList g, (x, c) <- IM.toList row]
  gridGetMaybeM (x, y) (IntMapGrid g) = pure $ IM.lookup x =<< IM.lookup y g
  gridGetM (x, y) g = unjust <$> gridGetMaybeM (x, y) g
  gridSetM a (x, y) (IntMapGrid g) = pure . IntMapGrid $ IM.adjust (IM.insert x a) y g
  gridModifyM f (x, y) (IntMapGrid g) = pure . IntMapGrid $ IM.adjust (IM.adjust f x) y g
  maxXYM (IntMapGrid g) = pure (maximum (IM.keys =<< IM.elems g), maximum $ IM.keys g)
  minXYM (IntMapGrid g) = pure (minimum (IM.keys =<< IM.elems g), minimum $ IM.keys g)
  mapCoordsM f (IntMapGrid g) = mkGridM $ [(f (x, y), c) | (y, row) <- IM.toList g, (x, c) <- IM.toList row]
  filterCoordsM f (IntMapGrid g) = mkGridM $ [((x, y), c) | (y, row) <- IM.toList g, (x, c) <- IM.toList row, f (x, y)]
  partitionCoordsM f g = do
    cs <- unGridM g
    g' <- mkGridM cs :: (Identity (Grid a))
    (l, r) <- partitionCoordsM f g'
    (l', r') <- bothM unGridM (l, r)
    bothM mkGridM (l', r')
  gridMemberM (x, y) g = isJust <$> gridGetMaybeM (x, y) g

newtype HashGrid' k a = HashGrid (HashMap k a) deriving (Eq, Ord, Show)

type HashGrid = HashGrid' Coord2

instance (GridCell a) => Griddable Identity HashGrid' Coord2 a where
  mkGridM = pure . HashGrid . HM.fromList
  unGridM (HashGrid g) = pure $ HM.toList g
  gridGetMaybeM c (HashGrid g) = pure $ HM.lookup c g
  gridGetM c (HashGrid g) = pure $ g HM.! c
  gridSetM a c (HashGrid g) = pure . HashGrid $ HM.insert c a g
  gridModifyM f c (HashGrid g) = pure . HashGrid $ HM.adjust f c g
  maxXYM (HashGrid g) = pure (maximum $ fst <$> HM.keys g, maximum $ snd <$> HM.keys g)
  minXYM (HashGrid g) = pure (minimum $ fst <$> HM.keys g, minimum $ snd <$> HM.keys g)
  mapCoordsM f (HashGrid g) = pure . HashGrid $ HM.mapKeys f g
  filterCoordsM f (HashGrid g) = pure . HashGrid $ HM.filterWithKey (\k _ -> f k) g
  partitionCoordsM f (HashGrid g) = pure . both (HashGrid . mkHashMap . unMap) $ M.partitionWithKey (\k _ -> f k) $ mkMap . unHashMap $ g
  gridMemberM c (HashGrid g) = pure $ HM.member c g

newtype VectorGrid' k a = VectorGrid (V.Vector (V.Vector a)) deriving (Eq, Ord, Show)

type VectorGrid = VectorGrid' Coord2

instance (GridCell a) => Griddable Identity VectorGrid' Coord2 a where
  -- TODO: need to respect indices for rotation
  mkGridM cs = pure . VectorGrid . V.fromList $ V.fromList <$> (snd <$$> (fmap (sortOn (fst . fst)) . sortOn (snd . fst . uhead) . groupOn (snd . fst) $ cs))
  unGridM g = do
    (x', y') <- minXYM g
    (x'', y'') <- maxXYM g
    sequence [((x, y),) <$> g <||!> (x, y) | x <- [x' .. x''], y <- [y' .. y'']]
  gridGetMaybeM (x, y) (VectorGrid g) = pure do
    row <- g V.!? y
    row V.!? x
  gridGetM (x, y) (VectorGrid g) = pure $ g V.! y V.! x
  gridSetM a (x, y) (VectorGrid g) =
    let row = g V.! y
     in pure . VectorGrid $ g V.// [(y, row V.// [(x, a)])]
  gridModifyM f (x, y) (VectorGrid g) =
    let row = g V.! y
     in pure . VectorGrid $ g V.// [(y, row V.// [(x, f (row V.! x))])]

  -- Need to modify this to support rotation
  maxXYM (VectorGrid g) = pure (V.length (g V.! 0) - 1, V.length g - 1)
  minXYM _ = pure (0, 0)

newtype AGrid' k a = AGrid (A.Array k a) deriving (Eq)

type AGrid a = AGrid' Coord2 a

instance (Monad m, GridCell a) => Griddable m AGrid' Coord2 a where
  mkGridM cs = return . AGrid $ A.array (minimum (fst <$> cs), maximum (fst <$> cs)) cs
  unGridM (AGrid g) = return $ A.assocs g
  gridGetMaybeM c@(x, y) (AGrid g) = do
    let ((ax, ay), (bx, by)) = A.bounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (AGrid g)
  gridGetM c (AGrid g) = return $ g A.! c
  gridSetM a c (AGrid g) = return . AGrid $ g A.// [(c, a)]
  gridModifyM f c (AGrid g) = return . AGrid $ g A.// [(c, f (g A.! c))]
  maxXYM (AGrid g) = return $ snd (A.bounds g)
  minXYM (AGrid g) = return $ fst (A.bounds g)

newtype ArrayGrid' k a = ArrayGrid (IOArray k a) deriving (Eq)

type ArrayGrid a = ArrayGrid' Coord2 a

instance (GridCell a) => Griddable IO ArrayGrid' Coord2 a where
  mkGridM cs =
    ArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (ArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (ArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (ArrayGrid g)
  gridGetM c (ArrayGrid g) = readArray g c
  gridSetM a c (ArrayGrid g) =
    ArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (ArrayGrid g) =
    ArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (ArrayGrid g) = snd <$> getBounds g
  minXYM (ArrayGrid g) = fst <$> getBounds g

newtype STUArrayGrid' s k a = STUArrayGrid (STA.STUArray s k a) deriving (Eq)

type STUArrayGrid s a = STUArrayGrid' s Coord2 a

instance (GridCell a, MArray (STA.STUArray s) a (ST s)) => Griddable (ST s) (STUArrayGrid' s) Coord2 a where
  mkGridM cs =
    STUArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (STUArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (STUArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (STUArrayGrid g)
  gridGetM c (STUArrayGrid g) = readArray g c
  gridSetM a c (STUArrayGrid g) =
    STUArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (STUArrayGrid g) =
    STUArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (STUArrayGrid g) = snd <$> getBounds g
  minXYM (STUArrayGrid g) = fst <$> getBounds g

newtype STArrayGrid' s k a = STArrayGrid (STA.STArray s k a) deriving (Eq)

type STArrayGrid s a = STArrayGrid' s Coord2 a

instance (GridCell a, MArray (STA.STArray s) a (ST s)) => Griddable (ST s) (STArrayGrid' s) Coord2 a where
  mkGridM cs =
    STArrayGrid <$> do
      a <- newArray_ (minimum (fst <$> cs), maximum (fst <$> cs))
      forM_ cs (uncurry $ writeArray a)
      return a
  unGridM (STArrayGrid g) = getAssocs g
  gridGetMaybeM c@(x, y) (STArrayGrid g) = do
    ((ax, ay), (bx, by)) <- getBounds g
    if x < ax || y < ay || x > bx || y > by then return Nothing else Just <$> gridGetM c (STArrayGrid g)
  gridGetM c (STArrayGrid g) = readArray g c
  gridSetM a c (STArrayGrid g) =
    STArrayGrid <$> do
      writeArray g c a
      return g
  gridModifyM f c (STArrayGrid g) =
    STArrayGrid <$> do
      a <- readArray g c
      writeArray g c (f a)
      return g
  maxXYM (STArrayGrid g) = snd <$> getBounds g
  minXYM (STArrayGrid g) = fst <$> getBounds g

newtype STVectorGrid' s k a = STVectorGrid (STV.STVector s (STV.STVector s a))

type STVectorGrid s a = STVectorGrid' s Coord2 a

-- TODO: support negative / sparse coords
instance (GridCell a) => Griddable (ST s) (STVectorGrid' s) Coord2 a where
  mkGridM cs = do
    let xs = fst . fst <$> cs
    let ys = snd . fst <$> cs
    let (minX, maxX) = (minimum xs, maximum xs)
    let (minY, maxY) = (minimum ys, maximum ys)
    vs <- STV.replicateM (maxY - minY + 1) $ STV.new (maxX - minX + 1)
    let g = STVectorGrid vs
    forM_ cs (\(c, a) -> gridSetM a c g)
    return g
  unGridM (STVectorGrid g) = do
    maxX <- (STV.length <$> STV.read g 0) <&> subtract 1
    let maxY = STV.length g - 1
    sequence
      [ ((x, y),) <$> ((g `STV.read` y) >>= (`STV.read` x))
        | (x, y) <- [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]]
      ]
  gridGetMaybeM c@(x, y) (STVectorGrid g) = do
    maxX <- STV.length <$> STV.read g 0
    let maxY = STV.length g
    if x > maxX || y > maxY || x < 0 || y < 0 then return Nothing else Just <$> gridGetM c (STVectorGrid g)
  gridGetM (x, y) (STVectorGrid g) = (g `STV.read` y) >>= (`STV.read` x)
  gridSetM a (x, y) grid@(STVectorGrid g) = do
    row <- g `STV.read` y
    STV.write row x a
    return grid
  gridModifyM f (x, y) grid@(STVectorGrid g) = do
    row <- g `STV.read` y
    STV.modify row f x
    return grid

  maxXYM (STVectorGrid g) = do
    maxX <- (STV.length <$> STV.read g 0) <&> subtract 1
    let maxY = STV.length g - 1
    return (maxX, maxY)
  minXYM _ = return (0, 0)

newtype IOVectorGrid' k a = IOVectorGrid (STV.IOVector (STV.IOVector a))

type IOVectorGrid a = IOVectorGrid' Coord2 a

-- TODO: support negative / sparse coords
instance (GridCell a) => Griddable IO IOVectorGrid' Coord2 a where
  mkGridM cs = do
    let xs = fst . fst <$> cs
    let ys = snd . fst <$> cs
    let (minX, maxX) = (minimum xs, maximum xs)
    let (minY, maxY) = (minimum ys, maximum ys)
    vs <- STV.replicateM (maxY - minY + 1) $ STV.new (maxX - minX + 1)
    let g = IOVectorGrid vs
    forM_ cs (\(c, a) -> gridSetM a c g)
    return g
  unGridM (IOVectorGrid g) = do
    maxX <- (STV.length <$> STV.read g 0) <&> subtract 1
    let maxY = STV.length g - 1
    sequence
      [ ((x, y),) <$> ((g `STV.read` y) >>= (`STV.read` x))
        | (x, y) <- [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]]
      ]
  gridGetMaybeM c@(x, y) (IOVectorGrid g) = do
    maxX <- STV.length <$> STV.read g 0
    let maxY = STV.length g
    if x > maxX || y > maxY || x < 0 || y < 0 then return Nothing else Just <$> gridGetM c (IOVectorGrid g)
  gridGetM (x, y) (IOVectorGrid g) = (g `STV.read` y) >>= (`STV.read` x)
  gridSetM a (x, y) grid@(IOVectorGrid g) = do
    row <- g `STV.read` y
    STV.write row x a
    return grid
  gridModifyM f (x, y) grid@(IOVectorGrid g) = do
    row <- g `STV.read` y
    STV.modify row f x
    return grid

  maxXYM (IOVectorGrid g) = do
    maxX <- (STV.length <$> STV.read g 0) <&> subtract 1
    let maxY = STV.length g - 1
    return $ fromXY (maxX, maxY)
  minXYM _ = return (0, 0)

-- To create a Cell, just supply a Bimap between char and cell
-- Or, one can override toChar and fromChar where there is some special logic
class (Eq a) => GridCell a where
  charMap :: Bimap a Char

  fromChar :: Char -> a
  default fromChar :: (Ord a) => Char -> a
  fromChar c = charMap BM.!> c

  toChar :: a -> Char
  default toChar :: (Ord a) => a -> Char
  toChar a = charMap BM.! a

data DotHash = Dot | Hash deriving (Eq, Ord, Bounded, Show)

instance GridCell Char where
  fromChar = id
  toChar = id
  charMap = BM.empty

instance GridCell Int where
  fromChar = digitToInt
  toChar = intToDigit
  charMap = BM.empty

instance GridCell Bool where
  charMap = mkBimap [(True, 'T'), (False, 'F')]

instance GridCell DotHash where
  charMap = BM.fromList [(Dot, '.'), (Hash, '#')]

newtype DigitCell = DigitCell (Fin Nat10) deriving (Eq, Ord, Bounded, Num, Show)

cellToInt :: DigitCell -> Integer
cellToInt (DigitCell d) = toInteger d

intToCell :: Int -> DigitCell
intToCell d = DigitCell (fromInteger $ toInteger d)

instance GridCell (Fin Nat10) where
  charMap = BM.fromList [((fromIntegral i), (intToDigit i)) | i <- [0 .. 9]]

instance GridCell DigitCell where
  charMap = BM.map DigitCell (charMap @(Fin Nat10))

newtype IntCell = IntCell Int deriving (Eq, Ord, Num, Show)

instance GridCell IntCell where
  charMap = BM.fromList [(IntCell i, intToDigit i) | i <- [0 .. 9]]

instance (Ord a, Integral a) => GridCell (Σ a) where
  charMap = BM.fromList [(Σ i, intToDigit $ fromIntegral i) | i <- [0 .. 9]]

readGridM :: (Griddable m g k a) => Text -> m (g k a)
readGridM = toGridM . lines

readGrid :: (Griddable Identity g k a) => Text -> g k a
readGrid = toGrid . lines

toGridM :: (Griddable m g k a) => [Text] -> m (g k a)
toGridM rows =
  mkGridM
    [ (fromXY (x, y), fromChar c)
      | (y, row) <- zip [0 ..] rows,
        (x, c) <- zip [0 ..] (T.unpack row)
    ]

toGrid :: (Griddable Identity g k a) => [Text] -> g k a
toGrid = runIdentity . toGridM

pointsM :: (Griddable m g k a) => [k] -> g k a -> m [a]
pointsM ps g = catMaybes <$> mapM (g <||?>) ps

points :: (Griddable Identity g k a) => [k] -> g k a -> [a]
points ps g = runIdentity $ pointsM ps g

class (Griddable m g k a) => IterGrid m g k d a where
  iterGridM' :: d -> g k a -> m [(k, a)]
  iterCoordsM' :: d -> g k a -> m [k]
  default iterCoordsM' :: d -> g k a -> m [k]
  iterCoordsM' d g = fst <$$> iterGridM' d g
  iterCellsM' :: d -> g k a -> m [a]
  default iterCellsM' :: d -> g k a -> m [a]
  iterCellsM' d g = snd <$$> iterGridM' d g

instance (Functor m, Griddable m g Coord2 a) => IterGrid m g Coord2 Dir2 a where
  iterGridM' DirDown g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [0 .. mx]] | y <- [0 .. my]]
  iterGridM' DirLeft g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [my, my - 1 .. 0]] | x <- [0 .. mx]]
  iterGridM' DirRight g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | y <- [0 .. my]] | x <- [0 .. mx]]
  iterGridM' DirUp g = do (mx, my) <- maxXYM g; fmap mconcat . mapM sequence $ [[((x, y),) <$> g <||!> (x, y) | x <- [mx, mx - 1 .. 0]] | y <- [my, my - 1 .. 0]]

iterGridM :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a, Default d) => g k a -> m [(k, a)]
iterGridM = iterGridM' (def @d)

iterCoordsM :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a, Default d) => g k a -> m [k]
iterCoordsM = iterCoordsM' (def @d)

iterCellsM :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a, Default d) => g k a -> m [a]
iterCellsM = iterCellsM' (def @d)

iterGrid' :: forall {g} {k} d {a} {acc}. (IterGrid Identity g k d a) => d -> g k a -> [(k, a)]
iterGrid' d = runIdentity . iterGridM' d

iterGrid :: forall {g} {k} d {a} {acc}. (IterGrid Identity g k d a, Default d) => g k a -> [(k, a)]
iterGrid = iterGrid' (def @d)

iterCoords' :: forall {g} {k} d {a} {acc}. (IterGrid Identity g k d a) => d -> g k a -> [k]
iterCoords' d g = runIdentity $ iterCoordsM' d g

iterCoords :: forall {m} {g} {k} d {a} {acc}. (IterGrid Identity g k d a, Default d) => g k a -> [k]
iterCoords = iterCoords' (def @d)

foldGridM' :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a) => d -> (acc -> (k, a) -> m acc) -> acc -> g k a -> m acc
foldGridM' d f init = foldM f init <=< iterGridM' d

foldGridM :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a, Default d) => (acc -> (k, a) -> m acc) -> acc -> g k a -> m acc
foldGridM = foldGridM' (def @d)

foldGrid :: forall {g} {k} d {a} {acc}. (IterGrid Identity g k d a, Default d) => (acc -> (k, a) -> acc) -> acc -> g k a -> acc
foldGrid f init = runIdentity . foldGridM @d ((return .) . f) init

foldCoordsM' :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a) => d -> (acc -> k -> m acc) -> acc -> g k a -> m acc
foldCoordsM' d f init = foldM f init <=< iterCoordsM' d

foldCoordsM :: forall {m} {g} {k} d {a} {acc}. (IterGrid m g k d a, Default d) => (acc -> k -> m acc) -> acc -> g k a -> m acc
foldCoordsM = foldCoordsM' (def @d)

foldCoords :: forall {g} {k} d {a} {acc}. (IterGrid Identity g k d a, Default d) => (acc -> k -> acc) -> acc -> g k a -> acc
foldCoords f init = runIdentity . foldCoordsM @d ((return .) . f) init

cropXM :: (Griddable m g k a) => Int -> Int -> g k a -> m (g k a)
cropXM i j g = do
  g' <- filterCoordsM (\c -> let (x, _) = toXY c in x >= i && x < j) g
  xO <- fst . toXY <$> minXYM g'
  mapCoordsM (mapXY $ first (subtract xO)) g'

cropX :: (Griddable Identity g k a) => Int -> Int -> g k a -> g k a
cropX i j g = runIdentity $ cropXM i j g

modifyCoordsM :: (Griddable m g k a) => (k -> k) -> g k a -> m (g k a)
modifyCoordsM f g = do
  (maxX, maxY) <- toXY <$> maxXYM g
  let xO = (maxX + 1) `div` 2
  let yO = (maxY + 1) `div` 2
  let toOrigin c = let (x, y) = toXY c in fromXY (x - xO, y - yO)
  let fromOrigin g =
        do
          (minX, minY) <- toXY <$> minXYM g
          mapCoordsM (\c -> let (x, y) = toXY c in fromXY (x - minX, y - minY)) g
  g' <- mapCoordsM (f . toOrigin) g
  fromOrigin g'

modifyCoords :: (Griddable Identity g k a) => (k -> k) -> g k a -> g k a
modifyCoords f g = runIdentity $ modifyCoordsM f g

variantsNubM :: (Griddable m g k a, Eq (g k a)) => g k a -> m [g k a]
variantsNubM g = nub <$> variantsM' g

variantsNub :: (Griddable Identity g k a, Eq (g k a)) => g k a -> [g k a]
variantsNub = runIdentity . variantsNubM

rotations :: (Griddable Identity g k a, Eq (g k a)) => g k a -> [g k a]
rotations = variants >>> pure >>> ([vId, r90, r180, r270] <*>)

variantsM' :: (Griddable m g k a) => g k a -> m [g k a]
variantsM' grid = do
  (maxX, _) <- toXY <$> maxXYM grid
  let isEven = even (maxX + 1)
      flipV (x, y) = (if isEven then negate x - 1 else negate x, y)
      flipH (x, y) = (x, if isEven then negate y - 1 else negate y)
      rot270 (x, y) = (y, if isEven then negate x - 1 else negate x)
      rot180 = rot270 . rot270
      rot90 = rot270 . rot270 . rot270
      mods = (.) <$> [id, flipH, flipV] <*> [id, rot90, rot180, rot270]
  sequence $ modifyCoordsM <$> (mapXY <$> mods) <*> pure grid

variants' :: (Griddable Identity g k a) => g k a -> [g k a]
variants' = runIdentity . variantsM'

data Variants g k a = Variants
  { vId :: g k a,
    r90 :: g k a,
    r180 :: g k a,
    r270 :: g k a,
    h0 :: g k a,
    h90 :: g k a,
    h180 :: g k a,
    h270 :: g k a,
    v0 :: g k a,
    v90 :: g k a,
    v180 :: g k a,
    v270 :: g k a
  }

variantsM :: (Griddable m g k a) => g k a -> m (Variants g k a)
variantsM grid = do
  vs <- variantsM' grid
  let [a, b, c, d, e, f, g, h, i, j, k, l] = vs
  return $ Variants a b c d e f g h i j k l

variants :: (Griddable Identity g k a) => g k a -> Variants g k a
variants = runIdentity . variantsM

gridLinesM :: (Griddable m g k a) => g k a -> m [[a]]
gridLinesM g = do
  (minX, minY) <- toXY <$> minXYM g
  (maxX, maxY) <- toXY <$> maxXYM g
  sequence [sequence [g <||!> fromXY (x, y) | x <- [minX .. maxX]] | y <- [minY .. maxY]]

gridLines :: (Griddable Identity g k a) => g k a -> [[a]]
gridLines = runIdentity . gridLinesM

prettyM :: (Griddable m g k a) => g k a -> m Text
prettyM grid = T.pack . intercalate "\n" <$> (fmap toChar <$$> gridLinesM grid)

pretty :: (Griddable Identity g k a) => g k a -> Text
pretty = runIdentity . prettyM

convolve ::
  ( Griddable Identity g Coord2 c,
    Griddable Identity g Coord2 d
  ) =>
  (Int, Int, Int, Int) ->
  (g Coord2 c -> d) ->
  g Coord2 c ->
  g Coord2 d
convolve (u, d, l, r) f g =
  mkGrid
    [ (c, f g')
      | c@(x', y') <- coords g,
        let g' =
              mapCoords (\(x, y) -> (x - x', y - y')) $
                filterCoords (\(x, y) -> x >= x' - l && y >= y' - u && x < x' + r && y < y' + d) g
    ]

convolveWith ::
  ( Griddable Identity g Coord2 c,
    Griddable Identity g Coord2 d
  ) =>
  (g Coord2 c -> g Coord2 c -> d) ->
  g Coord2 c ->
  g Coord2 c ->
  g Coord2 d
convolveWith f kernel g =
  let (w, h) = gridDims kernel
   in convolve (0, h, 0, w) (f kernel) g

data Perimeter k = Perimeter
  { pTop :: [k],
    pRight :: [k],
    pBottom :: [k],
    pLeft :: [k]
  }
  deriving (Eq, Ord, Show)

pFrom :: Perimeter Coord2 -> Dir2 -> [Coord2]
pFrom p DirDown = pTop p
pFrom p DirLeft = pRight p
pFrom p DirRight = pLeft p
pFrom p DirUp = pBottom p

perimeterM :: (Griddable m g Coord2 a) => g Coord2 a -> m (Perimeter Coord2)
perimeterM g = do
  (maxX, maxY) <- maxXYM g
  let top = [(x, 0) | x <- [0 .. maxX]]
  let right = [(maxX, y) | y <- [0 .. maxY]]
  let bottom = [(x, maxY) | x <- [0 .. maxX]]
  let left = [(0, y) | y <- [0 .. maxY]]
  return $ Perimeter top right bottom left

perimeter :: (Griddable Identity g Coord2 a) => g Coord2 a -> Perimeter Coord2
perimeter = runIdentity . perimeterM

instance (Griddable Identity Grid' k a) => Memberable k (Grid' k a) where
  a ∈ g = gridMember a g

instance Unionable (Grid a) where
  (Grid a) ∪ (Grid b) = Grid (a ∪ b)

instance Intersectable (Grid a) where
  (Grid a) ∩ (Grid b) = Grid (a ∩ b)

instance (Griddable Identity Grid' k a) => Gettable Grid' k a where
  (|!) = (||!)

instance (Griddable Identity Grid' k a) => MaybeGettable Grid' k a where
  (|?) = (||?)

instance (Griddable Identity Grid' k a) => ValueGettable (Grid' k a) k a where
  (|?>) = flip gridFind

instance (Griddable Identity Grid' k a) => Settable Grid' k a where
  (|.) = (||.)

instance (Griddable Identity Grid' k a) => Modifiable Grid' k a where
  (|~) = (||~)

instance (Griddable Identity VectorGrid' k a) => Gettable VectorGrid' k a where
  (|!) = (||!)

instance (Griddable Identity VectorGrid' k a) => MaybeGettable VectorGrid' k a where
  (|?) = (||?)

instance (Griddable Identity VectorGrid' k a) => Settable VectorGrid' k a where
  (|.) = (||.)

instance (Griddable Identity VectorGrid' k a) => Modifiable VectorGrid' k a where
  (|~) = (||~)

instance (Griddable Identity VectorGrid' k a) => Memberable k (VectorGrid' k a) where
  a ∈ g = gridMember a g

instance (Griddable Identity HashGrid' k a) => Gettable HashGrid' k a where
  (|!) = (||!)

instance (Griddable Identity HashGrid' k a) => MaybeGettable HashGrid' k a where
  (|?) = (||?)

instance (Griddable Identity HashGrid' k a) => Settable HashGrid' k a where
  (|.) = (||.)

instance (Griddable Identity HashGrid' k a) => Modifiable HashGrid' k a where
  (|~) = (||~)

instance (Griddable Identity HashGrid' k a) => Memberable k (HashGrid' k a) where
  a ∈ g = gridMember a g

class GridUnionable g a where
  gridUnionL :: g a -> g a -> g a
  gridUnionL = gridUnionWith const

  gridUnionR :: g a -> g a -> g a
  gridUnionR = gridUnionWith (flip const)

  gridUnionWith :: (a -> a -> a) -> g a -> g a -> g a

newtype MonoidalGrid m g k a = MonoidalGrid {unMonoidalGrid :: m (g k a)}

instance (Ord k) => GridUnionable (Grid' k) a where
  gridUnionWith f (Grid a) (Grid b) = Grid (M.unionWith f a b)

instance (Ord k) => GridUnionable (HashGrid' k) a where
  gridUnionWith f (HashGrid a) (HashGrid b) = HashGrid (HM.unionWith f a b)

instance (Ord k) => GridUnionable (VectorGrid' k) a where
  gridUnionWith f (VectorGrid a) (VectorGrid b) = VectorGrid (V.zipWith (V.zipWith f) a b)

instance (A.Ix k, Ord k) => GridUnionable (AGrid' k) a where
  gridUnionWith f (AGrid a) (AGrid b) = AGrid (zipWithA f a b)
    where
      zipWithA f xs ys = A.listArray (A.bounds xs) [f (xs A.! i) (ys A.! i) | i <- A.range (A.bounds xs)]

instance (Applicative m, GridUnionable (g k) a) => GridUnionable (MonoidalGrid m g k) a where
  gridUnionWith f a b = MonoidalGrid $ gridUnionWith @(g k) @a f <$> unMonoidalGrid a <*> unMonoidalGrid b

instance (Semigroup a, Griddable m g k a, GridUnionable (MonoidalGrid m g k) a) => Semigroup (MonoidalGrid m g k a) where
  (<>) = gridUnionWith (<>)

instance (Monoid a, Griddable m g k a, Semigroup (MonoidalGrid m g k a)) => Monoid (MonoidalGrid m g k a) where
  mempty = MonoidalGrid emptyGridM

idMonoidalGridSconcat :: (Semigroup a, Semigroup (MonoidalGrid Identity g k a)) => g k a -> g k a -> g k a
idMonoidalGridSconcat a b = runIdentity . unMonoidalGrid $ MonoidalGrid (pure a) <> MonoidalGrid (pure b)

idMonoidalGridMempty :: forall g k a. (Monoid a, Monoid (MonoidalGrid Identity g k a)) => g k a
idMonoidalGridMempty = runIdentity . unMonoidalGrid $ mempty @(MonoidalGrid Identity g k a)

instance (Semigroup a, Semigroup (MonoidalGrid Identity Grid' k a)) => Semigroup (Grid' k a) where
  (<>) = idMonoidalGridSconcat

instance (Monoid a, Monoid (MonoidalGrid Identity Grid' k a)) => Monoid (Grid' k a) where
  mempty = idMonoidalGridMempty

instance (Unionable (g k a)) => Unionable (MonoidalGrid Identity g k a) where
  a ∪ b = MonoidalGrid $ pure (runIdentity (unMonoidalGrid a) ∪ runIdentity (unMonoidalGrid b))

instance (Intersectable (g k a)) => Intersectable (MonoidalGrid Identity g k a) where
  a ∩ b = MonoidalGrid $ pure (runIdentity (unMonoidalGrid a) ∩ runIdentity (unMonoidalGrid b))

instance (Griddable m g k a) => Griddable m (MonoidalGrid m g) k a where
  mkGridM cs = return $ MonoidalGrid (mkGridM cs)
  unGridM g = unGridM =<< unMonoidalGrid g
  gridGetMaybeM k g = gridGetMaybeM k =<< unMonoidalGrid g
  gridGetM k g = gridGetM k =<< unMonoidalGrid g
  gridSetM a k g = return $ MonoidalGrid $ gridSetM a k =<< unMonoidalGrid g
  gridModifyM f k g = return $ MonoidalGrid $ gridModifyM f k =<< unMonoidalGrid g
  maxXYM g = maxXYM =<< unMonoidalGrid g
  minXYM g = minXYM =<< unMonoidalGrid g
  mapCoordsM f g = return $ MonoidalGrid $ mapCoordsM f =<< unMonoidalGrid g
  filterCoordsM f g = return $ MonoidalGrid $ filterCoordsM f =<< unMonoidalGrid g
  partitionCoordsM f g = both (MonoidalGrid . return) <$> (partitionCoordsM @m @g @k @a f =<< unMonoidalGrid g)
  gridMemberM k g = gridMemberM @m @g @k @a k =<< unMonoidalGrid g

wildEq :: (Eq a, Eq k, GridCell a, Griddable Identity g k a, GridUnionable (g k) a) => a -> g k a -> g k a -> Bool
wildEq wild k g
  | gridDims k /= gridDims g = False
  | otherwise =
      let f a b
            | a == wild || b == wild || a == b = wild
            | otherwise = a
       in all (== wild) (cells (gridUnionWith f k g))

x_x :: (Eq k, Griddable Identity g k Char, GridUnionable (g k) Char) => g k Char -> g k Char -> Bool
x_x = wildEq '_'

instance Unable (Grid' k) where
  un (Grid g) = un g

instance Convable (Grid' k a) (Map k a) where
  co (Grid g) = g

instance
  ( Applicative m,
    Monoid (m a),
    Semigroup (m a),
    Semigroup (m k),
    Swappable Map k a,
    SwapWithable Map m k a,
    Ord k
  ) =>
  Convable (Map k a) (Map a (m k))
  where
  co = swapcat

instance (Swappable Map k a, Ord a, Ord k) => Convable (Grid' k a) (Map a [k]) where
  co = swapcat . co @(Grid' k a) @(Map k a)

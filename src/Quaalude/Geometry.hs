module Quaalude.Geometry where

import Quaalude.Alias
import Quaalude.Collection
import Quaalude.Tuple
import Prelude hiding (filter)

lineSubtract :: ℤ² × ℤ² -> ℤ² × ℤ² -> [ℤ² × ℤ²]
lineSubtract m@((mx0, my0), (mx1, my1)) l@((lx0, ly0), (lx1, ly1))
  | overlapVV =
      traceShow ("VV", l, m) $
        let x = lx0
         in filter
              (\((_, y0), (_, y1)) -> y1 ≥ y0)
              [ ((x, min ly0 ly1), (x, min my0 my1 - 1)),
                ((x, max my0 my1 + 1), (x, max ly0 ly1))
              ]
  | overlapHH =
      traceShow ("HH", l, m) $
        let y = ly0
         in filter
              (\((x0, _), (x1, _)) -> x1 ≥ x0)
              [ ((min lx0 lx1, y), (min mx0 mx1 - 1, y)),
                ((max mx0 mx1 + 1, y), (max lx0 lx1, y))
              ]
  | overlapVH =
      traceShow ("VH", l, m) $
        let x = lx0
            y = my0
         in filter
              (\((_, y0), (_, y1)) -> y1 ≥ y0)
              [ ((x, min ly0 ly1), (x, y - 1)),
                ((x, y + 1), (x, max ly0 ly1))
              ]
  | overlapHV =
      traceShow ("HV", l, m) $
        let x = mx0
            y = ly0
         in filter
              (\((x0, _), (x1, _)) -> x1 ≥ x0)
              [ ((min lx0 lx1, y), (x - 1, y)),
                ((x + 1, y), (max lx0 lx1, y))
              ]
  | otherwise = [l]
  where
    lVert = lx0 ≡ lx1
    lHoriz = ly0 ≡ ly1
    mVert = mx0 ≡ mx1
    mHoriz = my0 ≡ my1
    sameX = lx0 ≡ mx0
    sameY = ly0 ≡ my0
    overlapVV = lVert ∧ mVert ∧ sameX
    overlapHH = lHoriz ∧ mHoriz ∧ sameY
    overlapVH = lVert ∧ mHoriz ∧ my0 ≥ min ly0 ly1 ∧ my1 ≤ max ly0 ly1 ∧ (min mx0 mx1 ≤ lx0) ∧ (max mx0 mx1 ≥ lx0)
    overlapHV = lHoriz ∧ mVert ∧ mx0 ≥ min lx0 lx1 ∧ mx1 ≤ max lx0 lx1 ∧ (min my0 my1 ≤ ly0) ∧ (max my0 my1 ≥ ly0)

intersectRectangles :: ℤ² × ℤ² -> ℤ² × ℤ² -> 𝔹
intersectRectangles ((ax0, ay0), (ax1, ay1)) ((bx0, by0), (bx1, by1)) =
  (min bx0 bx1 <= max ax0 ax1)
    ∧ (max bx0 bx1 >= min ax0 ax1)
    ∧ (min by0 by1 <= max ay0 ay1)
    ∧ (max by0 by1 >= min ay0 ay1)

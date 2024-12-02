module Quaalude.Math where

import Data.List qualified as L
import Data.Type.Nat (Nat (S), Nat9)

diff :: (Num a) => a -> a -> a
diff a b = abs (a - b)

-- Generators

triangular :: (Integral a) => a -> a
triangular n = n * (n + 1) `div` 2

-- Stats

median :: (Ord a) => [a] -> a
median xs = sort xs L.!! (length xs `div` 2)

mean :: (Integral a) => [a] -> a
mean xs = sum xs `div` fromIntegral (length xs)

-- Misc

type Nat10 = 'S Nat9

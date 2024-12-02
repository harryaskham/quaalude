module Quaalude.Math where

class Diffable a b where
  diff :: a -> b

instance (Num a) => Diffable (a, a) a where
  diff (a, b) = abs (a - b)

-- Generators

triangular :: (Integral a) => a -> a
triangular n = n * (n + 1) `div` 2

-- Stats

median :: (Ord a) => [a] -> a
median xs = sort xs !! (length xs `div` 2)

mean :: (Integral a) => [a] -> a
mean xs = sum xs `div` fromIntegral (length xs)

-- Misc

type Nat10 = 'S Nat9

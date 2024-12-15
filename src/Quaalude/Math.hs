module Quaalude.Math where

import Data.Basis
import Data.List qualified as L
import Data.Ratio
import GHC.TypeNats
import Quaalude.Alias
import Quaalude.Collection
import Relude.Unsafe qualified as U
import Text.Show
import Prelude hiding (drop)

diff :: (Num a) => a -> a -> a
diff a b = abs (a - b)

diffs :: (Num a) => [a] -> [a]
diffs xs = uncurry (-) <$> zip (drop 1 xs) xs

sgn :: (Num a, Ord a) => a -> a
sgn x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

-- Generators

triangular :: (Integral a) => a -> a
triangular n = n * (n + 1) `div` 2

-- Stats

median :: (Ord a) => [a] -> a
median xs = sort xs L.!! (length xs `div` 2)

mean :: (Integral a) => [a] -> a
mean xs = sum xs `div` fromIntegral (length xs)

-- Digits

class (Integral n) => ToDigit n a where
  toDigit :: a -> n

instance (Integral n, Integral m) => ToDigit n m where
  toDigit = fromIntegral

class FromDigit n a where
  fromDigit :: n -> a

instance (Integral n, Integral m) => FromDigit n m where
  fromDigit d
    | d < 0 ∨ d > 9 = error "Digit outside 0-9"
    | otherwise = fromIntegral d

class (Integral n) => ToDigits n a where
  toDigits :: a -> [n]

class (Integral n) => FromDigits n a where
  fromDigits :: [n] -> a

instance (Integral n, ToDigit n a) => ToDigits n [a] where
  toDigits = fmap toDigit

instance (FromDigit n a, Integral n) => FromDigits n [a] where
  fromDigits = fmap fromDigit

class (ToDigits n a, FromDigits n a) => OverDigits n a where
  overDigits :: ([n] -> [n]) -> a -> a
  overDigits2 :: ([n] -> [n] -> [n]) -> a -> a -> a

instance (ToDigits n a, FromDigits n a) => OverDigits n a where
  overDigits f = fromDigits . f . toDigits
  overDigits2 f a b = fromDigits (f (toDigits a) (toDigits b))

class (ToDigits n a) => ToDecimal n a where
  toDecimal :: a -> n

instance (ToDigit n a, FromDigit n a, Integral a, ToDigits n [a], Show n, Read n) => ToDecimal n [a] where
  toDecimal = U.read . mconcat . fmap (Prelude.show . fromIntegral) . toDigits @n @[a]

instance (ToDigit n a, ToDigits n [a]) => ToDigits n (Seq a) where
  toDigits = toDigits ∘ unSeq

instance (ToDigit n a, ToDecimal n [a]) => ToDecimal n (Seq a) where
  toDecimal = toDecimal ∘ unSeq

class (FromDigits n a) => FromDecimal n a where
  fromDecimal :: n -> a

instance (FromDigits n [a]) => FromDigits n (Seq a) where
  fromDigits = fromDigits ∘ toList

instance (FromDigits n [a], FromDigit n a, ToDigit n a, Show n) => FromDecimal n [a] where
  fromDecimal n = fromDigits @n @[a] $ fromIntegral ∘ digitToInt <$> Prelude.show n

instance (FromDecimal n [a], FromDigit n a, FromDigits n (Seq a)) => FromDecimal n (Seq a) where
  fromDecimal = mk . fromDecimal

class (ToDecimal n a, FromDecimal n a) => OverDecimal n a where
  overDecimal :: (n -> n) -> a -> a
  overDecimal2 :: (n -> n -> n) -> a -> a -> a

instance (ToDecimal n a, FromDecimal n a) => OverDecimal n a where
  overDecimal f = fromDecimal . f . toDecimal
  overDecimal2 f a b = fromDecimal (f (toDecimal a) (toDecimal b))

newtype Dec n a = Dec {unDec :: a} deriving (Functor, Applicative, Eq, Ord, Sizable)

type Dℤ = Dec ℤ

instance (ToDecimal n a, Show n, ToDecimal n (Dec n a), Show a) => Show (Dec n a) where
  show (Dec a) = "D" <> Text.Show.show (toDecimal @n a)

instance (ToDigits n a) => ToDigits n (Dec n a) where
  toDigits (Dec a) = toDigits a

instance (FromDigits n a) => FromDigits n (Dec n a) where
  fromDigits = Dec ∘ fromDigits

instance (ToDecimal n a, ToDigits n (Dec n a)) => ToDecimal n (Dec n a) where
  toDecimal (Dec a) = toDecimal a

instance (FromDecimal n a, FromDigits n (Dec n a)) => FromDecimal n (Dec n a) where
  fromDecimal = Dec . fromDecimal @n @a

instance (Num n, ToDecimal n (Dec n a), FromDecimal n (Dec n a), OverDecimal n (Dec n a)) => Num (Dec n a) where
  (+) = overDecimal2 @n @(Dec n a) (+)
  (-) = overDecimal2 @n @(Dec n a) (-)
  (*) = overDecimal2 @n @(Dec n a) (*)
  negate = overDecimal @n @(Dec n a) negate
  abs = overDecimal @n @(Dec n a) abs
  signum = overDecimal @n @(Dec n a) signum
  fromInteger = fromDecimal @n @(Dec n a) ∘ fromIntegral

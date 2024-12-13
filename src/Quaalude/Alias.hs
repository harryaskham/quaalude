module Quaalude.Alias where

import Data.Complex (Complex)
import Data.Fin (Fin)
import Data.Type.Nat (Nat (S), Nat9)

-- Numeric

type ℤ' = Int

type ℤ₆₄ = Int

type ℤ = Integer

type ℕ = Natural

type Nat10 = 'S Nat9

type ℕ₁₀ = Fin Nat10

type ℚ = Rational

type ℝ = Double

type ℝ² = (ℝ, ℝ)

type ℂ = Complex ℝ

type 𝔹 = Bool

-- Functional

bottom :: a
bottom = (⊥)

(⊥) :: a
(⊥) = error "Reached ⊥"

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

infixr 9 ∘

(⋅) :: (Num a) => a -> a -> a
(⋅) = (*)

infixl 7 ⋅

-- Boolean

(≡) :: (Eq a) => a -> a -> Bool
(≡) = (==)

infix 4 ≡

(≢) :: (Eq a) => a -> a -> Bool
(≢) = (/=)

(≤) :: (Ord a) => a -> a -> Bool
(≤) = (<=)

infix 4 ≤

(≥) :: (Ord a) => a -> a -> Bool
(≥) = (>=)

infix 4 ≥

(∧) :: Bool -> Bool -> Bool
(∧) = (&&)

infixr 3 ∧

(∨) :: Bool -> Bool -> Bool
(∨) = (||)

infixr 2 ∨

type family QuestionableF a where
  QuestionableF (Maybe a) = a

type family QuestionableFR a where
  QuestionableFR (Maybe a) = a

class Questionable a where
  (?) :: a -> QuestionableF a -> QuestionableFR a

infixl 1 ?

instance Questionable (Maybe a) where
  (?) = flip fromMaybe

(???) :: forall a. Bool -> a -> (a -> a)
True ??? a = flip const a
False ??? a = const a

infixl 1 ???

-- Monoidal

(<>.) :: (Monoid c) => (b -> [c]) -> (a -> b) -> (a -> c)
g <>. f = mconcat . g . f

infixr 9 <>.

(<>∘) :: (Monoid c) => (b -> [c]) -> (a -> b) -> (a -> c)
g <>∘ f = g <>. f

infixr 9 <>∘

(<>!) :: (Monoid a) => [a] -> a
(<>!) = mconcat

infixl 0 <>!

type ℤ² = (ℤ, ℤ)

type ℕ₁₀² = (ℕ₁₀, ℕ₁₀)

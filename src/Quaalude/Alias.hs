module Quaalude.Alias where

import Data.Complex (Complex)
import Data.Fin (Fin)
import Data.Type.Nat
import Relude.Unsafe qualified as U

-- Numeric

type ℤ' = Int

type ℤ₆₄ = Int

type ℤ = Integer

type ℤ² = (ℤ, ℤ)

type ℤ³ = (ℤ, ℤ, ℤ)

type ℤ⁴ = (ℤ, ℤ, ℤ, ℤ)

type ℤ⁵ = (ℤ, ℤ, ℤ, ℤ, ℤ)

type ℤ⁶ = (ℤ, ℤ, ℤ, ℤ, ℤ, ℤ)

type ℤ⁷ = (ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ)

type ℤ⁸ = (ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ)

type ℤ⁹ = (ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ)

type ℤ¹⁰ = (ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ, ℤ)

type ℕ = Natural

type Nat10 = 'S Nat9

type ℕ₁ = Fin Nat1

type ℕ₂ = Fin Nat2

type ℕ₃ = Fin Nat3

type ℕ₄ = Fin Nat4

type ℕ₅ = Fin Nat5

type ℕ₆ = Fin Nat6

type ℕ₇ = Fin Nat7

type ℕ₈ = Fin Nat8

type ℕ₉ = Fin Nat9

type ℕ₁₀ = Fin Nat10

type ℕ₁₀² = (ℕ₁₀, ℕ₁₀)

type ℚ = Rational

type ℚ² = (ℚ, ℚ)

type ℝ = Double

type ℝ² = (ℝ, ℝ)

type ℂ = Complex ℝ

type 𝔹 = Bool

class Inf a where
  infinity :: a
  (∞) :: a
  (∞) = infinity
  ꝏ :: a
  ꝏ = infinity

pattern Ꝏ <- infinity

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

(<>?) :: (Monoid a) => [Maybe a] -> [a]
(<>?) = catMaybes

infixl 0 <>?

(±) :: (Num a) => a -> a
(±) = signum

infixl 0 ±

(⇱) :: (Bifunctor p) => (a -> b) -> p a c -> p b c
(⇱) = first

infixr 0 ⇱

(⇲) :: (Bifunctor p) => p b a -> (a -> c) -> p b c
(⇲) = flip second

infixr 0 ⇲

(#) :: (Monad m) => a -> m a
(#) = return

infixl 0 #

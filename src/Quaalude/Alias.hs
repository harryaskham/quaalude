module Quaalude.Alias where

import Data.Complex (Complex)

(⊥) :: a
(⊥) = error "Reached ⊥"

type ℤ' = Int

type ℤ₆₄ = Int

type ℤ = Integer

type ℕ = Natural

type ℚ = Rational

type ℝ = Double

type ℂ = Complex ℝ

type 𝔹 = Bool

(∘) :: (b -> c) -> (a -> b) -> a -> c
(∘) = (.)

infixr 9 ∘

(⋅) :: (Num a) => a -> a -> a
(⋅) = (*)

infixl 7 ⋅

(≡) :: (Eq a) => a -> a -> Bool
(≡) = (==)

infix 4 ≡

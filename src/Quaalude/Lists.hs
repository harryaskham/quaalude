module Quaalude.Lists where

import Data.HList
import GHC.TypeNats
import Quaalude.Alias
import Quaalude.Collection

class Nothings l where
  nothings :: HList l

instance Nothings '[] where
  nothings = HNil

instance (Nothings xs) => Nothings (Maybe x' ': xs) where
  nothings = HCons Nothing (nothings @xs)

data HomList (n :: Nat) a where
  HomNil :: forall {a}. HomList 0 a
  HomCons :: forall n {a}. a -> HomList (n - 1) a -> HomList n a

type family HomConsF a b where
  HomConsF a a = HomList 2 a
  HomConsF a (HomList n a) = HomList (n + 1) a

type family HomConsEvidenceF n a where
  HomConsEvidenceF 2 a =
    (HomConsF a a ~ HomList 2 a)
  HomConsEvidenceF n (HomList n a) =
    ((n + 1) - 1 ~ n, HomConsF a (HomList n a) ~ HomList (n + 1) a)

type family HomConsEvidenceC na :: Constraint where
  HomConsEvidenceC (SNat n, a) = HomConsEvidenceF n a

type family HomConsEvidenceT na

class (HomConsEvidenceC (HomConsEvidenceT (SNat n, a))) => HomConsEvidence n a

instance (HomConsEvidenceC (HomConsEvidenceT (SNat n, a))) => HomConsEvidence n a

type family HomConsLHS b where
  HomConsLHS (HomList _ a) = a
  HomConsLHS a = a

class (n ~ (n + 1 - 1)) => NatSuccPred n

instance (n ~ (n + 1 - 1)) => NatSuccPred n

class HomConsC a where
  (~:~) :: (NatSuccPred n) => a -> HomList n a -> HomList (n + 1) a
  (~:~) = HomCons
  (~:|) :: a -> a -> HomList 2 a
  a ~:| b = HomCons a (HomCons b HomNil)

instance HomConsC a

infixr 5 ~:~

data Question (c :: * -> Constraint) a = Question a

type a :~:~ b = HomConsF a b

infixr 5 :~:~

class ConstC (cf :: (k -> Constraint)) (a :: k)

instance (cf a) => ConstC cf a

type family AllEqF a :: * -> Constraint where
  AllEqF (HomList _ _) = ConstC AllEq

type family a :≡ b where
  a :≡ () = Question AllEq (HomList 1 a)
  a :≡ Question c (HomList n a) = Question c (HomList (n + 1) a)

infix 4 :≡

class AllEq a where
  allEq :: a -> Bool

instance {-# OVERLAPS #-} AllEq (HomList 0 a) where
  allEq _ = True

instance {-# OVERLAPS #-} (Eq a, AllEq (HomList (n - 1) a)) => AllEq (HomList n a) where
  allEq (HomCons a rest@(HomCons b _)) = a ≡ b ∧ allEq @(HomList (n - 1) a) rest
  allEq (HomCons _ HomNil) = True

type family QuestionResultF c where
  QuestionResultF AllEq = Bool

type family RunQuestionC q :: Constraint where
  RunQuestionC (Question c a) = c a

type family RunQuestionF q where
  RunQuestionF (Question c a) = a -> QuestionResultF c

class RunQuestion q where
  runQuestion :: (RunQuestionC q) => RunQuestionF q

instance Unable (HomList 0) where
  un HomNil = traceUn "HomList 0" $ []

instance (Unable (HomList (n - 1))) => Unable (HomList n) where
  un (HomCons a as) = traceUn "HomList n" $ a : un as

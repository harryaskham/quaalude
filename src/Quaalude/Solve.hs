module Quaalude.Solve where

import Data.Basis
import Quaalude.Alias
import Quaalude.Collection
import Quaalude.Unary
import Quaalude.Util
import System.IO.Unsafe (unsafePerformIO)
import Z3.Monad

z3 :: Z3 a -> a
z3 = unsafePerformIO . evalZ3

z3IO :: (MonadIO m) => Z3 a -> m a
z3IO = liftIO ∘ evalZ3

type family Z3SolverF a where
  Z3SolverF [a] = [Z3SolverF a]
  Z3SolverF (Σ a) = [Z3SolverF a]
  Z3SolverF a = a

class Z3Solver a where
  eval1 :: Model -> AST -> Z3 (Maybe (Z3SolverF a))

instance Z3Solver ℚ where
  eval1 = evalReal

instance Z3Solver ℤ where
  eval1 = evalInt

type family SolveForF a where
  SolveForF [_] = [AST]
  SolveForF (Σ _) = [AST]
  SolveForF (a, b) = (SolveForF a, SolveForF b)
  SolveForF _ = AST

class (Integral i, SolveFor i, SolveFor a) => Minimize i a where
  minimize :: String -> Z3 (SolveForF i, SolveForF a) -> Z3 (Maybe (i, a))

instance (Integral i, SolveFor i, Show i, Show a, SolveFor a, SolveForF i ~ AST) => Minimize i a where
  minimize name ia =
    let go :: Int -> Maybe (i, a) -> Z3 (SolveForF i, SolveForF a) -> Z3 (Maybe (i, a))
        go i lastM next = do
          print ("Minimize iteration " <> show i)
          print ("Last solution: " <> show lastM)
          v@(nAST, aAST) <- next
          na <- solve @(i, a) v
          case na of
            Nothing -> do
              print "No more solutions, last was:"
              print lastM
              pure lastM
            Just (n, o) ->
              go (i + 1) na $ do
                n' <- z3Int (fromIntegral n)
                assert =<< mkLt nAST n'
                pure (nAST, aAST)
     in go 0 Nothing ia

class SolveFor a where
  solve :: SolveForF a -> Z3 (Maybe a)

instance SolveFor ℚ where
  solve ast = join ∘ snd <$> withModel (\model -> eval1 @ℚ model ast)

instance SolveFor ℤ where
  solve ast = join ∘ snd <$> withModel (\model -> eval1 @ℤ model ast)

instance (Z3Solver a, Z3SolverF a ~ a) => SolveFor [a] where
  solve asts = snd <$> withModel (\model -> catMaybes <$> mapM (eval1 @a model) asts)

instance (SolveFor a, SolveFor b) => SolveFor (a, b) where
  solve (a, b) = do
    sa <- solve @a a
    sb <- solve @b b
    return $ bisequence (sa, sb)

z3WithModel :: (Applicative z3, MonadZ3 z3) => (Model -> z3 a) -> z3 (Result, Maybe a)
z3WithModel = withModel

traceModel :: Model -> Z3 a -> Z3 a
traceModel model a = do
  s <- (modelToString model)
  traceShow s $ a

printModel :: Model -> Z3 ()
printModel model = do
  s <- modelToString model
  print s

printAST :: AST -> Z3 ()
printAST a = print =<< astToString a

fromZ3Maybe :: Maybe a -> a
fromZ3Maybe Nothing = error "fromZ3Maybe: no solve"
fromZ3Maybe (Just a) = a

z3Check :: Z3 Result
z3Check = check

z3EvalInt :: (MonadZ3 z3) => EvalAst z3 Integer
z3EvalInt = evalInt

z3EvalReal :: (MonadZ3 z3) => EvalAst z3 Rational
z3EvalReal = evalReal

z3EvalBool :: (MonadZ3 z3) => EvalAst z3 Bool
z3EvalBool = evalBool

z3Rational :: (MonadZ3 z3) => Rational -> z3 AST
z3Rational = mkRational

z3Int :: (MonadZ3 z3) => Integer -> z3 AST
z3Int = mkInteger

z3Bool :: (MonadZ3 z3) => Bool -> z3 AST
z3Bool = mkBool

z3RealVar' :: (MonadZ3 z3) => Symbol -> z3 AST
z3RealVar' = mkRealVar

z3IntVar' :: (MonadZ3 z3) => Symbol -> z3 AST
z3IntVar' = mkIntVar

z3BoolVar' :: (MonadZ3 z3) => Symbol -> z3 AST
z3BoolVar' = mkBoolVar

z3RealVar :: (MonadZ3 z3) => String -> z3 AST
z3RealVar s = mkRealVar =<< mkStringSymbol s

z3IntVar :: (MonadZ3 z3) => String -> z3 AST
z3IntVar s = mkIntVar =<< mkStringSymbol s

z3BoolVar :: (MonadZ3 z3) => String -> z3 AST
z3BoolVar s = mkBoolVar =<< mkStringSymbol s

z3Add :: (MonadZ3 z3) => [AST] -> z3 AST
z3Add = mkAdd

z3Sub :: (MonadZ3 z3) => [AST] -> z3 AST
z3Sub = mkSub

z3Eq :: (MonadZ3 z3) => AST -> AST -> z3 AST
z3Eq = mkEq

z3Gt :: (MonadZ3 z3) => AST -> AST -> z3 AST
z3Gt = mkGt

z3Ge :: (MonadZ3 z3) => AST -> AST -> z3 AST
z3Ge = mkGe

z3And :: (MonadZ3 z3) => [AST] -> z3 AST
z3And = mkAnd

z3Assert :: (MonadZ3 z3) => AST -> z3 ()
z3Assert = assert


module Quaalude.Reducible where

import Data.HList (HExtend (HExtendR), HList (..), HasField, Record (..), Tagged (..), typeRep)
import Data.Tagged (Tagged)
import Data.Text qualified as T
import GHC.TypeLits
import Named
import Quaalude.Described

data Reduced f where
  Reduced :: f -> Reduced f

newtype Terminal b = Terminal {unTerminal :: b}

type family ReducedF f where
  ReducedF (f :@ d) = ReducedF f :@ d
  ReducedF (Reduced f) = Reduced f
  ReducedF (Terminal b) = Terminal b
  ReducedF (a -> b) = Reduced (a -> ReducedF b)
  ReducedF b = Terminal b

class Reducible f where
  reduce :: f -> ReducedF f

instance Reducible (Reduced f) where
  reduce = id

instance Reducible (Terminal b) where
  reduce = id

instance (Reducible b) => Reducible (a -> b) where
  reduce f =
    let f' :: a -> ReducedF b
        f' a = reduce (f a)
     in Reduced f'

-- Return the function type without the :@ description annotations on arguments
type family StripDescriptionsF f where
  StripDescriptionsF (f :@ _) = StripDescriptionsF f
  StripDescriptionsF (Reduced f) = Reduced (StripDescriptionsF f)
  StripDescriptionsF (a :@ _ -> b) = a -> StripDescriptionsF b
  StripDescriptionsF b = b

class StripDescriptions f where
  stripDescriptions :: f -> StripDescriptionsF f
  default stripDescriptions :: (f ~ StripDescriptionsF f) => f -> StripDescriptionsF f
  stripDescriptions b = b

instance StripDescriptions (Terminal b)

instance StripDescriptions (NamedF g a name -> b) where
  stripDescriptions = id

instance
  ( StripDescriptions a,
    StripDescriptionsF a ~ StripDescriptionsF (Tagged (ArgDesc description) a)
  ) =>
  StripDescriptions (Tagged (ArgDesc description) a)
  where
  stripDescriptions (Tagged a) = stripDescriptions @a a

instance
  ( StripDescriptions b,
    (NamedF g a name -> StripDescriptionsF b)
      ~ StripDescriptionsF (Tagged (ArgDesc description) (NamedF g a name) -> b)
  ) =>
  StripDescriptions (Tagged (ArgDesc description) (NamedF g a name) -> b)
  where
  stripDescriptions f =
    let f' a = (let b = f (Tagged @(ArgDesc description) a) in stripDescriptions b) in f'

instance (StripDescriptions f) => StripDescriptions (Reduced f) where
  stripDescriptions (Reduced f) = Reduced (stripDescriptions f)

-- A list of ordered argument types
type family ArgL f where
  ArgL (Reduced (a -> b)) = a ': ArgL b
  ArgL (a -> b) = ArgL (ReducedF (a -> b))
  ArgL _ = '[]

type family CurryF f where
  CurryF (HList '[] -> b) = b
  CurryF ((HList (a ': b)) -> c) = a -> CurryF (HList b -> c)

type UncurryF f = HList (ArgL f) -> ReturnF f

class CurryHList f where
  uncurryHList :: f -> UncurryF f
  default uncurryHList :: (CurryHList (ReducedF f), Reducible f, UncurryF f ~ UncurryF (ReducedF f)) => f -> UncurryF f
  uncurryHList = uncurryHList . reduce
  curryHList :: UncurryF f -> f

instance CurryHList (Terminal a) where
  uncurryHList (Terminal a) = \HNil -> a
  curryHList f = Terminal $ f HNil

instance (CurryHList b) => CurryHList (Reduced (a -> b)) where
  uncurryHList (Reduced f) =
    \(HCons a b) ->
      let f' = f a
          fb = uncurryHList @b f'
       in fb b
  curryHList f =
    Reduced
      ( \a ->
          let f' bArgs = f (HCons a bArgs)
           in curryHList @b f'
      )

-- A record from annotated argument name to argument
type family ArgR f where
  ArgR (Reduced ((a :@ _) -> b)) = ArgR (Reduced (a -> b))
  ArgR (Reduced (NamedF g a name -> b)) = Tagged name (g a) ': ArgR b
  ArgR (Terminal _) = '[]
  ArgR (f :@ _) = ArgR f
  ArgR (a -> b) = ArgR (ReducedF (a -> b))
  ArgR _ = '[]

type HasArg f name v = HasField name (Record (ArgR f)) v

-- Get the arguments defined using :!
class (Reducible f) => RequiredArgs f where
  requiredArgs :: [String]
  default requiredArgs :: (RequiredArgs (ReducedF f)) => [String]
  requiredArgs = requiredArgs @(ReducedF f)
  requiredArgs' :: f -> [String]
  requiredArgs' _ = requiredArgs @f

instance (Reducible b, RequiredArgs (ReducedF (a -> b))) => RequiredArgs (a -> b) where
  requiredArgs = requiredArgs @(ReducedF (a -> b))

instance RequiredArgs (Terminal b) where
  requiredArgs = []

instance (RequiredArgs (Reduced (a -> b))) => RequiredArgs (Reduced ((a :@ description) -> b)) where
  requiredArgs = requiredArgs @(Reduced (a -> b))

instance (RequiredArgs b, KnownSymbol (name :: Symbol)) => RequiredArgs (Reduced (name :! a -> b)) where
  requiredArgs = symbolVal (Proxy @name) : requiredArgs @b

instance (RequiredArgs b) => RequiredArgs (Reduced (a :? name -> b)) where
  requiredArgs = requiredArgs @b

-- The return type of a function with or without annotations
-- Will not return Terminal
type family ReturnF f where
  ReturnF (Reduced (_ -> b)) = ReturnF b
  ReturnF (Terminal b) = b
  ReturnF f = ReturnF (ReducedF f)

class (ReturnF f ~ out) => Return f out where
  returnF :: Proxy out
  default returnF :: (Return (ReducedF f) out) => Proxy out
  returnF = returnF @(ReducedF f)
  returnF' :: f -> Proxy out
  returnF' _ = returnF @f

instance (Reducible (a -> b), Return (ReducedF (a -> b)) out) => Return (a -> b) out

instance Return (Terminal out) out where
  returnF = Proxy @out

instance (Return b out, out ~ ReturnF (a -> b)) => Return (Reduced (a -> b)) out where
  returnF = returnF @b @out

-- Evaluate an annotated function using a record of named arguments
class AppArgR f argR out where
  appArgR :: f -> Record argR -> out

instance AppArgR b '[] b where
  appArgR b _ = b

instance
  ( argR ~ (arg ': argR'),
    argR ~ ArgR f,
    arg ~ Tagged name (g a),
    HasField name (Record argR) (g a),
    WithParam (NamedF g a name) f f',
    ArgR f' ~ argR',
    out ~ ReturnF f,
    out ~ ReturnF f',
    AppArgR f' argR' out
  ) =>
  AppArgR f (arg ': argR') out
  where
  appArgR f (Record (HCons (Tagged (v :: g a)) (argR' :: (HList argR')))) =
    let (f' :: f') = f Named.! paramF (Name @name) (v :: g a)
     in appArgR @f' @argR' @out f' (Record argR')

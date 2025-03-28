
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

class ShowArgR (a :: [*]) where
  showArgR' :: [Text]

  showArgR :: Text
  showArgR = parenLinesArgs (showArgR' @a)

  showArgRT' :: [Text]

  showArgRT :: Text
  showArgRT = parenLinesArgs (showArgRT' @a)

  showArgRD' :: [Text]

  showArgRD :: Text
  showArgRD = parenLinesArgs (showArgRD' @a)

instance ShowArgR '[] where
  showArgR' = []
  showArgRT' = []
  showArgRD' = []

instance (ShowArg a, ShowArgR as) => ShowArgR (a ': as) where
  showArgR' = showArg @a : showArgR' @as
  showArgRD' = showArgD @a : showArgRD' @as
  showArgRT' = showArgT @a : showArgRT' @as

class ShowArgRKW (a :: [*]) where
  showArgRKW' :: Record a -> [Text]

  showArgRKW :: Record a -> Text
  showArgRKW argR = parenLinesArgs (showArgRKW' argR)

instance ShowArgRKW '[] where
  showArgRKW' _ = []

instance (ShowArgKW (Tagged n (g x)) g x, ShowArgRKW as) => ShowArgRKW (Tagged n (g x) ': as) where
  showArgRKW' (Record (HCons (Tagged gx) xs)) =
    showArgKW @(Tagged n (g x)) @g gx : showArgRKW' @as (Record xs)

class (ShowArgR (ArgR f)) => ShowArgs f where
  showArgs :: Text
  showArgs = showArgR @(ArgR f)

  showArgs' :: f -> Text
  showArgs' _ = showArgs @f

  showArgsT :: Text
  showArgsT = showArgRT @(ArgR f)

  showArgsT' :: f -> Text
  showArgsT' _ = showArgsT @f

  showArgsD :: Text
  showArgsD = showArgRD @(ArgR f)

  showArgsD' :: f -> Text
  showArgsD' _ = showArgsD @f

instance (ShowArgR (ArgR f)) => ShowArgs f

class (ShowArgRKW (ArgR f)) => ShowArgsKW f where
  showArgsKW :: Record (ArgR f) -> Text
  showArgsKW = showArgRKW @(ArgR f)

instance (ShowArgRKW (ArgR f)) => ShowArgsKW f

class (Typeable (ReturnF f)) => ShowReturn f where
  showReturn :: Text
  showReturn = show (typeRep (Proxy @(ReturnF f)))
  showReturn' :: f -> Text
  showReturn' _ = showReturn @f

instance (Typeable (ReturnF f)) => ShowReturn f

nameArgsReturns :: forall t f. (FName' t f, ShowReturn f) => Text -> Text
nameArgsReturns shownArgs = nameArgs @t @f shownArgs <> " -> " <> showReturn @f

nameArgsValue :: forall t f x. (FName' t f, Show x) => Text -> x -> Text
nameArgsValue shownArgs x = nameArgs @t @f shownArgs <> " = " <> show x

class (IsFunction' t f, FName' t f, ShowArgs f, ShowReturn f) => ShowF' t f where
  showF :: Text
  showF = nameArgsReturns @t @f (showArgs @f)
  showF' :: f -> Text
  showF' _ = showF @t @f
  showFT :: Text
  showFT = nameArgsReturns @t @f (showArgsT @f)
  showFT' :: f -> Text
  showFT' _ = showFT @t @f

instance (IsFunction' t f, FName' t f, ShowArgs f, ShowReturn f) => ShowF' t f

class (ShowF' f f) => ShowF f

instance (ShowF' f f) => ShowF f

class (IsFunction' t f, FName' t f, FDescription' t f, ShowArgs f, ShowReturn f) => ShowFD' t f where
  showFD :: Text
  showFD = nameArgsReturns @t @f (showArgsD @f)
  showFD' :: f -> Text
  showFD' _ = showFD @t @f

instance (IsFunction' t f, FName' t f, FDescription' t f, ShowArgs f, ShowReturn f) => ShowFD' t f

class (ShowFD' f f) => ShowFD f

instance (ShowFD' f f) => ShowFD f

class (IsFunction' t f, FName' t f, ShowArgsKW f) => ShowFKW' t f where
  showFKW :: Record (ArgR f) -> Text
  showFKW argR = nameArgs @t @f (showArgsKW @f argR)
  showFKW' :: f -> Record (ArgR f) -> Text
  showFKW' _ = showFKW @t @f

instance (IsFunction' t f, FName' t f, ShowArgsKW f) => ShowFKW' t f

class (ShowFKW' f f) => ShowFKW f

instance (ShowFKW' f f) => ShowFKW f

class (IsFunction' t f, FName' t f, ShowArgsKW f) => ShowFKWR' t f where
  showFKWR :: Record (ArgR f) -> Text -> Text
  showFKWR argR shownOut = showFKW @t @f argR <> " = " <> shownOut

instance (IsFunction' t f, FName' t f, ShowArgsKW f) => ShowFKWR' t f

class (ShowFKWR' f f) => ShowFKWR f

instance (ShowFKWR' f f) => ShowFKWR f
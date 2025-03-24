module Quaalude.Described where

import Data.HList (HExtend (HExtendR), HList (..), HasField, Record (..), Tagged (..), typeRep)
import Data.Tagged (Tagged)
import Data.Text qualified as T
import GHC.TypeLits
import Named

data ArgDesc (desc :: Symbol) = ArgDesc

data FnDesc (desc :: Symbol) = FnDesc

data FnName (name :: Symbol) = FnName

-- i.e. for args like (argname :! type :@ "some description")
type arg :@ (description :: Symbol) = Tagged (ArgDesc description) arg

type (name :: Symbol) :#> fn = Tagged (FnName name) fn

infixr 5 :#>

type (description :: Symbol) :@> fn = Tagged (FnDesc description) fn

infixr 5 :@>

class ArgName a where
  argName :: Text

instance (KnownSymbol n) => ArgName (Tagged n x) where
  argName = T.pack (symbolVal (Proxy @n))

instance (KnownSymbol n) => ArgName (NamedF g x n) where
  argName = T.pack (symbolVal (Proxy @n))

instance (ArgName a) => ArgName (a :@ x) where
  argName = argName @a

class ArgType a where
  argType :: Text

type family UnIdentity a where
  UnIdentity (Identity x) = x
  UnIdentity x = x

instance (Typeable (UnIdentity x)) => ArgType (Tagged n x) where
  argType = T.show (typeRep (Proxy @(UnIdentity x)))

instance (Typeable (UnIdentity x)) => ArgType (n :! x) where
  argType = T.show (typeRep (Proxy @(UnIdentity x)))

instance (Typeable (UnIdentity x)) => ArgType (n :? x) where
  argType = "?" <> T.show (typeRep (Proxy @(UnIdentity x)))

instance (ArgType a) => ArgType (a :@ d) where
  argType = argType @a

class ArgTypeIs a x

instance ArgTypeIs (Tagged n x) x

instance ArgTypeIs (NamedF g a x) x

instance ArgTypeIs (NamedF g a x :@ d) x

class ArgDescription a where
  argDescription :: Maybe Text

instance ArgDescription (Tagged n x) where
  argDescription = Nothing

instance ArgDescription (NamedF g a x) where
  argDescription = Nothing

instance (KnownSymbol d) => ArgDescription (a :@ d) where
  argDescription = Just (T.pack (symbolVal (Proxy @d)))

class (ArgName a, ArgType a, ArgDescription a) => ShowArg a where
  showArg :: Text
  showArg = argName @a

  showArgT :: Text
  default showArgT :: Text
  showArgT = showArg @a <> ":" <> argType @a

  showArgD :: Text
  showArgD = showArgT @a <> maybe "" (\d -> " (" <> d <> ")") (argDescription @a)

instance (ArgName a, ArgType a, ArgDescription a) => ShowArg a

class (ShowArg a, ArgType a, ArgTypeIs a (g x)) => ShowArgKW a g x where
  showArgKW :: g x -> Text

instance (ShowArg a, ArgType a, ArgTypeIs a (Identity x), Show x) => ShowArgKW a Identity x where
  showArgKW (Identity x) = showArg @a <> "=" <> T.show x

instance (ShowArg a, ArgType a, ArgTypeIs a (Maybe x), Show x) => ShowArgKW a Maybe x where
  showArgKW (Just x) = showArg @a <> "=" <> T.show x
  showArgKW Nothing = showArg @a <> "=null"

parenArgs :: [Text] -> Text
parenArgs args = "(" <> T.intercalate ", " args <> ")"

parenLinesArgs :: [Text] -> Text
parenLinesArgs args =
  T.intercalate
    "\n"
    [ "(",
      T.intercalate ",\n" (("  " <>) <$> args),
      ")"
    ]

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
  showReturn = T.show (typeRep (Proxy @(ReturnF f)))
  showReturn' :: f -> Text
  showReturn' _ = showReturn @f

instance (Typeable (ReturnF f)) => ShowReturn f

class IsFunction' t f where
  getFunction :: t -> f

instance IsFunction' f f where
  getFunction = id

class (IsFunction' f f) => IsFunction f

instance (IsFunction' f f) => IsFunction f

instance (IsFunction' f f') => IsFunction' (Tagged (FnName n) f) f' where
  getFunction (Tagged f) = getFunction @f @f' f

instance (IsFunction' f f') => IsFunction' (Tagged (FnDesc d) f) f' where
  getFunction (Tagged f) = getFunction @f @f' f

class (IsFunction' t f) => FName' t f where
  fName :: Text
  default fName :: (FName f) => Text
  fName = fName @f @f

class (FName' f f) => FName f

instance (FName' f f) => FName f

instance
  ( IsFunction' (Tagged (FnName n) (Tagged (FnName d) f)) f,
    KnownSymbol n
  ) =>
  FName' (Tagged (FnName n) (Tagged (FnName d) f)) f
  where
  fName = T.pack (symbolVal (Proxy @n))

class (IsFunction' t f) => FDescription' t f where
  fDescription :: Maybe Text
  default fDescription :: (FDescription f) => Maybe Text
  fDescription = fDescription @f @f

instance
  ( IsFunction' (Tagged (FnName n) (Tagged (FnName d) f)) f,
    KnownSymbol d
  ) =>
  FDescription' (Tagged (FnName n) (Tagged (FnName d) f)) f
  where
  fDescription = Just $ T.pack (symbolVal (Proxy @d))

class (FDescription' f f) => FDescription f

instance (FDescription' f f) => FDescription f

nameArgs :: forall t f. (FName' t f) => Text -> Text
nameArgs shownArgs = fName @t @f <> shownArgs

nameArgsReturns :: forall t f. (FName' t f, ShowReturn f) => Text -> Text
nameArgsReturns shownArgs = nameArgs @t @f shownArgs <> " -> " <> showReturn @f

nameArgsValue :: forall t f x. (FName' t f, Show x) => Text -> x -> Text
nameArgsValue shownArgs x = nameArgs @t @f shownArgs <> " = " <> T.show x

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

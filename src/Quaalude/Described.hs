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
  argType = show (typeRep (Proxy @(UnIdentity x)))

instance (Typeable (UnIdentity x)) => ArgType (n :! x) where
  argType = show (typeRep (Proxy @(UnIdentity x)))

instance (Typeable (UnIdentity x)) => ArgType (n :? x) where
  argType = "?" <> show (typeRep (Proxy @(UnIdentity x)))

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
  showArgKW (Identity x) = showArg @a <> "=" <> show x

instance (ShowArg a, ArgType a, ArgTypeIs a (Maybe x), Show x) => ShowArgKW a Maybe x where
  showArgKW (Just x) = showArg @a <> "=" <> show x
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

nameArgs :: forall t f. (FName' t f) => Text -> Text
nameArgs shownArgs = fName @t @f <> shownArgs
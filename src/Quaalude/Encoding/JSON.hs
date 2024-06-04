module Quaalude.Encoding.JSON where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Null),
    eitherDecode,
    encode,
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (insert)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Text qualified as T
import Data.Typeable (Proxy (Proxy))
import Deriving.Aeson
  ( CamelToSnake,
    CustomJSON,
    FieldLabelModifier,
    OmitNothingFields,
    RejectUnknownFields,
    StringModifier (..),
    SumUntaggedValue,
  )
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude hiding (One, Parser, insert, keys)

-- Anything with a Prefix can be JSON'd
-- This can be derived using: deriveJSONPrefixed ''Name
-- This can also be derived using: deriveAuto ''Name (will use the Name as the prefix)

class (KnownSymbol (RemovePrefix a)) => JSONPrefixed a where
  type RemovePrefix a :: Symbol

data StripPrefixLowerFirst t

instance (KnownSymbol k) => StringModifier (StripPrefixLowerFirst k) where
  getStringModifier s =
    let (p : refix) = fromMaybe <*> stripPrefix (symbolVal (Proxy @k)) $ s
     in (toLower p : refix)

type OmitNothingUntagged (prefix :: Symbol) = CustomJSON '[OmitNothingFields, SumUntaggedValue]

type PrefixedCamel (prefix :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefixLowerFirst prefix)]

type PrefixedCamelOmitNothing (prefix :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefixLowerFirst prefix), OmitNothingFields]

type PrefixedCamelOmitNothingUntagged (prefix :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefixLowerFirst prefix), OmitNothingFields, SumUntaggedValue]

type PrefixedCamelRejectUnknown (prefix :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefixLowerFirst prefix), RejectUnknownFields]

type PrefixedCamelRejectUnknownOmitNothing (prefix :: Symbol) = CustomJSON '[FieldLabelModifier (StripPrefixLowerFirst prefix), RejectUnknownFields]

type PrefixedSnakeOmitNothing (prefix :: Symbol) = CustomJSON '[FieldLabelModifier '[StripPrefixLowerFirst prefix, CamelToSnake], OmitNothingFields]

type PrefixedSnakeOmitNothingUntagged (prefix :: Symbol) = CustomJSON '[FieldLabelModifier '[StripPrefixLowerFirst prefix, CamelToSnake], OmitNothingFields, SumUntaggedValue]

type PrefixedSnakeRejectUnknown (prefix :: Symbol) = CustomJSON '[FieldLabelModifier '[StripPrefixLowerFirst prefix, CamelToSnake], RejectUnknownFields]

type PrefixedSnakeRejectUnknownOmitNothing (prefix :: Symbol) = CustomJSON '[FieldLabelModifier '[StripPrefixLowerFirst prefix, CamelToSnake], RejectUnknownFields]

instance (JSONPrefixed t) => JSONPrefixed (t Identity) where
  type RemovePrefix (t Identity) = RemovePrefix t

unsafeCoerceViaJSON :: forall b a. (ToJSON a, FromJSON b) => a -> b
unsafeCoerceViaJSON a = case eitherDecode (encode a) of
  Left e -> error $ T.pack e
  Right b -> b

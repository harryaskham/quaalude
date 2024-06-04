module Quaalude.Encoding.JSON.TH where

import Barbies
import Barbies.Bare (Covered)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON)
import Database.Beam.Schema.Tables (Nullable)
import Deriving.Aeson (StringModifier)
import Deriving.Aeson.Stock (PrefixedSnake)
import Language.Haskell.TH (Dec, Name, Q, Type (VarT), conT, mkName)
import Quaalude.Encoding.JSON
  ( JSONPrefixed (..),
    PrefixedCamel,
    PrefixedCamelOmitNothingUntagged,
    PrefixedSnakeOmitNothingUntagged,
  )
import Quaalude.TH
import Prelude hiding (Type)

deriveJSONPrefixed :: [Name] -> Q [Dec]
deriveJSONPrefixed names = do
  prefix <- getJsonPrefix names
  deriveJSONPrefixed' prefix names

deriveJSONPrefixed' :: String -> [Name] -> Q [Dec]
deriveJSONPrefixed' prefix names =
  [d|
    instance JSONPrefixed ($(joinNames names)) where
      type RemovePrefix ($(joinNames names)) = $(stringToSymbol prefix)
    |]

deriveJSONVia :: ([Name] -> Q Type) -> [Name] -> Q [Dec]
deriveJSONVia viaT names =
  mconcat
    <$> forM
      [[t|ToJSON ($(joinNames names))|], [t|FromJSON ($(joinNames names))|]]
      (\instT -> [d|deriving via $(viaT names) ($(joinNames names)) instance $instT|])

deriveJSONViaForTag :: Name -> Maybe (Q Type) -> Q Type -> ([Name] -> Q Type) -> [Name] -> Q [Dec]
deriveJSONViaForTag toFrom constraintsM tag viaT names =
  case constraintsM of
    Nothing -> [d|deriving via $(viaT names) ($(joinNames names) ($tag)) instance $(conT toFrom) ($(joinNames names) ($tag))|]
    Just constraints -> [d|deriving via $(viaT names) ($(joinNames names) ($tag)) instance ($constraints) => $(conT toFrom) ($(joinNames names) ($tag))|]

deriveToJSONViaForTag :: Bool -> Name -> ([Name] -> Q Type) -> [Name] -> Q [Dec]
deriveToJSONViaForTag True tagName = deriveJSONViaForTag ''ToJSON Nothing [t|Nullable $(conT tagName)|]
deriveToJSONViaForTag False tagName = deriveJSONViaForTag ''ToJSON Nothing [t|$(conT tagName)|]

deriveFromJSONViaForTag :: Bool -> Name -> ([Name] -> Q Type) -> [Name] -> Q [Dec]
deriveFromJSONViaForTag True tagName = deriveJSONViaForTag ''FromJSON Nothing [t|Nullable $(conT tagName)|]
deriveFromJSONViaForTag False tagName = deriveJSONViaForTag ''FromJSON Nothing [t|$(conT tagName)|]

deriveJSONRemovingPrefix' :: Name -> ([Name] -> Q [Dec]) -> [Name] -> Q [Dec]
deriveJSONRemovingPrefix' prefixClass jsonPrefixedDeriver names = do
  deriveJP <- jsonPrefixedDeriver names
  deriveToFrom <- deriveJSONVia (\names -> [t|$(conT prefixClass) (RemovePrefix ($(joinNames names)))|]) names
  return $ deriveJP <> deriveToFrom

deriveJSONSnakeRemovingPrefix :: Name -> Q [Dec]
deriveJSONSnakeRemovingPrefix name = do
  deriveJSONRemovingPrefix'
    ''PrefixedSnakeOmitNothingUntagged
    deriveJSONPrefixed
    [name]

deriveJSONCamelRemovingPrefix :: Name -> Q [Dec]
deriveJSONCamelRemovingPrefix name = do
  deriveJSONRemovingPrefix'
    ''PrefixedCamelOmitNothingUntagged
    deriveJSONPrefixed
    [name]

-- Derive the JSONPrefix instance for e.g. SiteT, then ToJSON instances for e.g. SiteT Identity and SiteT Auto, but only FromJSON for SiteT Identity
deriveJSONRemovingPrefixForTags' :: Name -> ([Name] -> Q [Dec]) -> [Name] -> [Name] -> [Name] -> [Name] -> [Name] -> Q [Dec]
deriveJSONRemovingPrefixForTags'
  prefixClass
  jsonPrefixedDeriver
  names
  toJSONTags
  fromJSONTags
  toJSONTagsNullable
  fromJSONTagsNullable = do
    deriveJP <- jsonPrefixedDeriver names
    forToJSONTags <- forM toJSONTags $ \tag -> deriveToJSONViaForTag False tag (\names -> [t|$(conT prefixClass) (RemovePrefix ($(joinNames names)))|]) names
    forFromJSONTags <- forM fromJSONTags $ \tag -> deriveFromJSONViaForTag False tag (\names -> [t|$(conT prefixClass) (RemovePrefix ($(joinNames names)))|]) names
    forToJSONTagsNullable <- forM toJSONTagsNullable $ \tag -> deriveToJSONViaForTag True tag (\names -> [t|$(conT prefixClass) (RemovePrefix ($(joinNames names)))|]) names
    forFromJSONTagsNullable <- forM fromJSONTagsNullable $ \tag -> deriveFromJSONViaForTag True tag (\names -> [t|$(conT prefixClass) (RemovePrefix ($(joinNames names)))|]) names
    return $ deriveJP <> mconcat forToJSONTags <> mconcat forFromJSONTags <> mconcat forToJSONTagsNullable <> mconcat forFromJSONTagsNullable

deriveJSONSnakeRemovingPrefixForTags :: Name -> [Name] -> [Name] -> [Name] -> [Name] -> Q [Dec]
deriveJSONSnakeRemovingPrefixForTags name = deriveJSONRemovingPrefixForTags' ''PrefixedSnake deriveJSONPrefixed [name]

deriveJSONCamelRemovingPrefixForTags :: Name -> [Name] -> [Name] -> [Name] -> [Name] -> Q [Dec]
deriveJSONCamelRemovingPrefixForTags name = deriveJSONRemovingPrefixForTags' ''PrefixedCamel deriveJSONPrefixed [name]

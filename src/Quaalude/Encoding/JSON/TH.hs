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

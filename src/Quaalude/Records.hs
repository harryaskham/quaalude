module Quaalude.Records where

import Data.Aeson.Casing (snakeCase)
import Data.HList
import Data.HList.HList
import Data.HList.Record
import GHC.TypeLits

-- Convenience function to drop i.e. _llamaRequest... from field names when deriving JSON.
-- i.e. use dropPrefix "LlamaRequest" for the above
dropPrefix :: String -> String -> String
dropPrefix prefix s = if prefix `isPrefixOf` s then drop (length prefix) s else s

-- for use as:
-- deriveJSON defaultOptions {fieldLabelModifier = snakeCaseNoPrefix "_recordName"} ''RecordName
snakeCaseNoPrefix :: String -> String -> String
snakeCaseNoPrefix prefix = snakeCase . dropPrefix prefix

-- Unwrap a Record to its underlying typelevel list
type family UnRecord record :: [*] where
  UnRecord (Record r) = r
  UnRecord e = TypeError ('Text "UnRecord: " ':<>: 'ShowType e)

-- Records whose values are all Maybe and can be created with all Nothing values
class NothingsR l where
  nothingsR :: Record l

instance NothingsR '[] where
  nothingsR = emptyRecord

instance
  ( NothingsR xs,
    HLabelSet (Label name ': LabelsOf xs),
    HAllTaggedLV xs
  ) =>
  NothingsR (Tagged name (Maybe x) ': xs)
  where
  nothingsR = Label @name .=. Nothing .*. nothingsR @xs

-- Requires that all fields in 'to' are optional so we can populate them if not present in 'from'
class (NothingsR to) => ConvertRecord from to where
  convertRecord :: Record from -> Record to
  default convertRecord :: (from ~ to) => Record from -> Record to
  convertRecord = id

instance
  ( NothingsR to,
    HAllTaggedLV to,
    HLabelSet (LabelsOf to),
    UnionSymRec to from both,
    H2ProjectByLabels (LabelsOf to) both to onlyFrom
  ) =>
  ConvertRecord from to
  where
  convertRecord from =
    -- Union preferring the Just values from 'from' over the Nothing values from 'to' on overlap
    let both :: Record both
        both = snd (unionSR (nothingsR @to) from)
        -- Retain only to-labels from one side of the symmetric union
        to :: Record to
        to = hProjectByLabels (Proxy @(LabelsOf to)) both
     in to
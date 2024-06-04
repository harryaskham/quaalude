module Quaalude.Records where

import Data.Aeson.Casing (snakeCase)

-- Convenience function to drop i.e. _llamaRequest... from field names when deriving JSON.
-- i.e. use dropPrefix "LlamaRequest" for the above
dropPrefix :: String -> String -> String
dropPrefix prefix s = if prefix `isPrefixOf` s then drop (length prefix) s else s

-- for use as:
-- deriveJSON defaultOptions {fieldLabelModifier = snakeCaseNoPrefix "_recordName"} ''RecordName
snakeCaseNoPrefix :: String -> String -> String
snakeCaseNoPrefix prefix = snakeCase . dropPrefix prefix
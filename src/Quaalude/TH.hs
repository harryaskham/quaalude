module Quaalude.TH where

import Control.Lens (Traversable (traverse))
import Data.Char (toLower)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.List qualified as L
import Data.Text qualified as T
import GHC.TypeLits (Symbol)
import Language.Haskell.TH
import Network.HTTP.Req (defaultHttpConfig)
import Quaalude.Collection (Packable (..))
import Quaalude.Grid
import Quaalude.Tracers
import Quaalude.Util
import Relude.Unsafe qualified as U
import Text.ParserCombinators.Parsec
import Prelude hiding (Type)

-- Build a function that runs all days, converts results to Text,
-- and returns [(day, part, result)]
runAllDays :: Q Exp
runAllDays =
  return . ListE $
    ( \(d, p) ->
        TupE
          [ Just (LitE $ IntegerL d),
            Just (LitE $ IntegerL p),
            Just
              ( AppE
                  (VarE 'show)
                  (VarE (mkName $ concat (["Day", show d, ".part", show p] :: [String])))
              )
          ]
    )
      <$> [(d, p) | d <- [1 .. 25], p <- [1, 2]]

-- Literal inputs; use TH to embed the input at compile time

class EmbedInput a where
  input :: a -> Q Exp

grid :: Int -> Q Exp
grid day = [e|readGrid ($(input day) :: Text)|]

aoc :: Int -> Q Exp
aoc = input @Int

aocx :: Int -> Q Exp
aocx = input @Example . Example

aocxn :: Int -> Int -> Q Exp
aocxn = (input @Example .) . ExampleN

instance EmbedInput Int where
  input day = do
    path <- makeRelativeToProject (inputPath day)
    AppE (VarE 'decodeUtf8) <$> embedFile path

data Example = Example Int | ExampleN Int Int

instance EmbedInput Example where
  input (Example day) = do
    path <- makeRelativeToProject (exampleInputPath day)
    AppE (VarE 'decodeUtf8) <$> embedFile path
  input (ExampleN day n) = do
    path <- makeRelativeToProject (exampleInputNPath day n)
    AppE (VarE 'decodeUtf8) <$> embedFile path

inputS :: (EmbedInput a) => a -> Q Exp
inputS day = AppE (VarE 'T.unpack) <$> input day

gridsT :: (Griddable Identity g k a, Packable t Text) => t -> [g k a]
gridsT s = readGrid <$> T.splitOn "\n\n" (pack s)

grids :: (Int -> Q Exp) -> Int -> Q Exp
grids inputFn day = AppE (VarE 'gridsT) <$> inputFn day

gridM :: (Int -> Q Exp) -> Int -> Q Exp
gridM inputFn day = AppE (VarE 'readGridM) <$> inputFn day

gridsTM :: (Griddable m g k a, Packable t Text) => t -> m [g k a]
gridsTM s = traverse readGridM $ T.splitOn "\n\n" (pack s)

gridsM :: (Int -> Q Exp) -> Int -> Q Exp
gridsM inputFn day = AppE (VarE 'gridsTM) <$> inputFn day

fileLine :: Q Exp
fileLine = do
  loc <- location
  let file = loc_filename loc
      line = fst $ loc_start loc
  [|(file, line)|]

-- Gets the prefix of a constructor name.
-- Doesn't actually need the type name therefore, since it only strips based on the
-- matchable constructor name.
-- Adds an underscore and lowercases the first letter of the constructor name.
-- e.g. "SiteAuthor" -> "_siteAuthor"
-- If the fields are not underscore-prefixed, returns just the camelCased constructor name.
-- e.g. "SiteAuthor" -> "siteAuthor"
-- If the fields are not prefixed with the constructor name, returns an empty string.
getJsonPrefix :: [Name] -> Q String
getJsonPrefix (n : _) = do
  rfs <- getRecordFields <$> reify n
  case rfs of
    [(ctorName, fieldNamesTypes)] ->
      do
        let camelCtorName = toLower (U.head ctorName) : U.tail ctorName
            fieldNames = fst <$> fieldNamesTypes
            prefix =
              case L.nub (U.head <$> fieldNames) of
                ['_'] -> "_" ++ camelCtorName
                _ -> camelCtorName
        if not (all (prefix `isPrefixOf`) fieldNames)
          then return ""
          else return prefix
    rfs -> error $ "getJsonPrefix (no fields found for " <> tshow n <> "): Only single-constructor records supported, got " <> tshow rfs
  where
    getRecordFields :: Info -> [(String, [(String, String)])]
    getRecordFields (TyConI (DataD _ _ _ _ cons _)) = concatMap getRF' cons
    getRecordFields (TyConI (NewtypeD _ _ _ _ con _)) = getRF' con
    getRecordFields _ = []
    getRF' :: Con -> [(String, [(String, String)])]
    getRF' (RecC name fields) = [(nameBase name, map getFieldInfo fields)]
    getRF' _ = []
    getFieldInfo :: (Name, Strict, Type) -> (String, String)
    getFieldInfo (name, _, ty) = (nameBase name, show ty)

stringToSymbol :: String -> Q Type
stringToSymbol s =
  let sLit = pure $ LitT (StrTyLit s)
   in [t|$sLit :: Symbol|]

joinNames :: [Name] -> Q Type
joinNames names = do
  conTs <- traverse conT names
  return $ L.foldl1 (\t c -> AppT t c) conTs

nameSuffix :: Name -> String -> Name
nameSuffix name suffix = mkName (nameBase name <> suffix)

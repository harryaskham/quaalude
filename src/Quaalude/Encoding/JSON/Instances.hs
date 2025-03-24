module Quaalude.Encoding.JSON.Instances where

import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Encoding.Internal as E (InArray, comma, econcat, key, retagEncoding)
import Data.Aeson.Types
import Data.HList
import Data.HList.Record
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Quaalude.Collection
import Quaalude.Tuple
import GHC.TypeLits
import qualified Data.Text as T
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

tupleToJSON ∷ forall a t u. (ToJSON a, TupSnoc u a t, ToJSON t) => u -> Value
tupleToJSON u =
    let (a, t) :: (a, t) = tupSnoc @u @a @t u
    in case toJSON @t t of
          Array tValues -> Array (toJSON @a a `V.cons` tValues)
          _ -> error "toJSON on Tuple tail returned non-array"

tupleFromJSON ∷ forall a t u. (FromJSON a, FromJSON t, TupCons a t u) => Value -> Parser u
tupleFromJSON = \case
    Array v | not (V.null v) -> do
        let (headVal, tailVals) = (V.head v, V.tail v)
        a <- parseJSON @a headVal
        t <- parseJSON @t (Array tailVals)
        return $ tupCons @a @t @u a t
    _ -> fail "Expected non-empty array for tuple"

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)

instance (ToJSON a, ToJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z))
  => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  toJSON =
    tupleToJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y)

instance (FromJSON a, FromJSON (b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z))
  => FromJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) where
  parseJSON =
    tupleFromJSON @a @(b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z) @(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z)

newtype TaggedKV k v = TaggedKV { unTaggedKV :: Tagged k v }

instance (KnownSymbol k, ToJSON v) => ToJSON (TaggedKV k v) where
  toJSON (TaggedKV (Tagged v)) =
    let key = fromString $ symbolVal (Proxy @k)
     in object [key .= toJSON v]

instance (KnownSymbol k, FromJSON v) => FromJSON (TaggedKV k v) where
  parseJSON =
    let (key :: Key) = fromString $ symbolVal (Proxy @k)
     in withObject "Record" (\o -> TaggedKV . (Label @k .=.) <$> o .: key)

instance ToJSON (Record '[]) where
  toJSON _ = emptyObject

instance (ToJSON (Record rest), ToJSON (TaggedKV k v))
  => ToJSON (Record (Tagged k v ': rest)) where
  toJSON (Record (HCons (Tagged v) rest)) =
    let vValue = toJSON @(TaggedKV k v) (TaggedKV @k (Tagged v))
        restValue = toJSON @(Record rest) (Record rest)
    in case (vValue, restValue) of
         (Object vObj, Object restObj) -> Object (vObj <> restObj)
         _ -> error "Expected object when encoding Record"


instance FromJSON (Record '[]) where
  parseJSON _ = return emptyRecord

instance (FromJSON (Record rest), FromJSON a, KnownSymbol name)
  => FromJSON (Record (Tagged name a ': rest)) where
  parseJSON = withObject "Record" $ \obj -> do
    let keyName = symbolVal (Proxy @name)
    let key = Key.fromString keyName
    val <- obj .: key
    Record rest <- parseJSON @(Record rest) (Object obj)
    return . Record $ HCons (Tagged val) rest
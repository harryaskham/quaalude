module Quaalude
  ( module X,
    Seq (..),
    deleteFindMin,
  )
where

import Control.Lens as X
  ( at,
    indices,
    itraversed,
    ix,
    makeLenses,
    mapped,
    plate,
    traversed,
    use,
    view,
    (%%~),
    (%=),
    (%~),
    (**~),
    (*~),
    (+~),
    (-~),
    (.=),
    (.~),
    (//~),
    (?~),
    (^.),
    (^..),
    (^^~),
    (^~),
  )
import Control.Monad.Memo as X hiding (isNothing)
import Control.Monad.ST as X
import Data.Bits as X (Bits (bit, bitSize, bitSizeMaybe, complement, isSigned, popCount, rotate, shift, testBit, xor, (.&.), (.|.)))
import Data.Default as X
import Data.Distributive as X
import Data.Either as X
import Data.HList as X (HList (..), (.*.))
import Data.List.GroupBy as X
import Data.Map.Strict as X hiding (delete, deleteFindMin, drop, empty, filter, foldl', foldr, fromList, keys, lookup, map, mapMaybe, null, size, split, splitAt, take, toList, (!?), (\\))
import Data.PQueue.Prio.Min (deleteFindMin)
import Data.Ratio as X
import Data.Sequence (Seq (..))
-- WIP; import manually
-- import Quaalude.Type as X

import Data.Text as X (stripPrefix)
import Data.Tuple.Extra as X (fst3, snd3, thd3)
import Data.Tuple.Solo as X
import Data.Variant as X
import Data.Variant.Types as X hiding (Index, Length, Product)
import Data.Vector as X (Vector)
import GHC.TypeLits as X hiding (natVal, someNatVal)
import Numeric.Search.Range as X
import Quaalude.Alias as X
import Quaalude.Bits as X
import Quaalude.Collection as X
import Quaalude.Compose as X
import Quaalude.Coord as X
import Quaalude.Grid as X
import Quaalude.Math as X
import Quaalude.Megaparsec as X
import Quaalude.Records as X
import Quaalude.TH as X
import Quaalude.Tracers as X
import Quaalude.Tuple as X
import Quaalude.Unary as X
import Quaalude.Util as X hiding (count)
import Quaalude.Variadic as X
import Relude as X hiding (drop, filter, group, many, optional, splitAt, swap, take, (<|>))
import System.IO.Unsafe as X
import Text.ParserCombinators.Parsec as X hiding (State)
import Text.RawString.QQ as X

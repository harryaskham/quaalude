module Quaalude.Test.Spec where

import Quaalude
import Test.Hspec

data A = A deriving (Eq, Show, Ord)

a :: A = A

data B = B deriving (Eq, Show, Ord)

b :: B = B

instance Convable A B where
  co A = B

g :: ".@" ▦ ℤ² = emptyGrid

spec :: Spec
spec = do
  describe ("mk (,)") do
    it "makes [a] to (Solo a)" $ mk @Solo @A [a] `shouldBe` MkSolo a
    it "makes [a, a] to (a, a)" $ mk [a, a] `shouldBe` (a, a)
    it "makes [a, a, a] to (a, a, a)" $ mk [a, a, a] `shouldBe` (a, a, a)
    it "makes [a, a, a, a] to (a, a, a, a)" $ mk [a, a, a, a] `shouldBe` (a, a, a, a)
  describe ("co [] (,)") do
    it "converts [] to ()" $ co [] `shouldBe` ()
    it "converts [a] to (Solo a)" $ co [a] `shouldBe` MkSolo a
    it "converts [a, a] to (a, a)" $ co [a, a] `shouldBe` (a, a)
    it "converts [a, a, a] to (a, a, a)" $ co [a, a, a] `shouldBe` (a, a, a)
    it "converts [a, a, a, a] to (a, a, a, a)" $ co [a, a, a, a] `shouldBe` (a, a, a, a)
  describe ("co (,) []") do
    it "converts [] to ()" $ co [] `shouldBe` ()
    it "converts [a] to (Solo a)" $ co [a] `shouldBe` MkSolo a
    it "converts [a, a] to (a, a)" $ co [a, a] `shouldBe` (a, a)
    it "converts [a, a, a] to (a, a, a)" $ co [a, a, a] `shouldBe` (a, a, a)
    it "converts [a, a, a, a] to (a, a, a, a)" $ co [a, a, a, a] `shouldBe` (a, a, a, a)
  describe ("co as mk") do
    it "co-mks [a] to Set a" $ co [a] `shouldBe` mkSet [a]
    it "co-mks [a] to Set b" $ co [a] `shouldBe` mkSet [b]
  describe ("co nested") do
    it "converts ([],[]) to ((,),(,))" $ co ([a], [a, a]) `shouldBe` (MkSolo a, (a, a))
    it "converts Solo ([],[]) to Solo ((,),(,))" $ co (MkSolo ([a], [a, a])) `shouldBe` (MkSolo (MkSolo a, (a, a)))
    it "converts [([],[])] to NonEmpty ((,),(,))" $ co [([a], [a, a])] `shouldBe` (MkSolo a, (a, a)) :| []
  describe ("co self") do
    it "converts a to a" $ co a `shouldBe` a
    it "converts (Solo a) to (Solo a)" $ co (MkSolo a) `shouldBe` (MkSolo a)
    it "converts (a,a) to (a,a)" $ co (a, a) `shouldBe` (a, a)
    it "converts Grid to Grid" $ co g `shouldBe` g

  describe ("DP") do
    it "shows truncated DP <0" $ showDP 3 (-10.12345) `shouldBe` "-10.123"
    it "shows truncated DP <1" $ showDP 3 0.12345 `shouldBe` "0.123"
    it "shows truncated DP 3" $ showDP 3 123.456789 `shouldBe` "123.457"
    it "shows truncated DP 1" $ showDP 1 123.456789 `shouldBe` "123.5"
    it "shows expanded DP" $ showDP 3 123.4 `shouldBe` "123.4"

  describe ("nubOn") do
    it "nubOn size" $ nubOn size [[1], [2], [3, 4], [4, 5], [5, 6, 7]] `shouldBe` [[1], [3, 4], [5, 6, 7]]

  describe ("MaxSet") do
    it "transposes MaxSet" $ ((MaxSet @(Integer, Integer) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ⊤) `shouldBe` (MaxSet (5, 10) (mkSet [(0, 0), (2, 7), (5, 10)]))
    it "mirrors MaxSet H" $ ((MaxSet @(Integer, Integer) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ◐) `shouldBe` (MaxSet (10, 5) (mkSet [(10, 0), (3, 2), (0, 5)]))
    it "mirrors MaxSet V" $ ((MaxSet @(Integer, Integer) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ◓) `shouldBe` (MaxSet (10, 5) (mkSet [(0, 5), (7, 3), (10, 0)]))
    it "rotates MaxSet CW" $ ((MaxSet @(Integer, Integer) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ↻) `shouldBe` (MaxSet (5, 10) (mkSet [(0, 10), (2, 3), (5, 0)]))
    it "rotates MaxSet CCW" $ ((MaxSet @(Integer, Integer) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ↺) `shouldBe` (MaxSet (5, 10) (mkSet [(5, 0), (3, 7), (0, 10)]))

  describe ("BoundedSet") do
    it "transposes BoundedSet" $ toOrigin ((BoundedSet @(Integer, Integer) (0, 0) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ⊤) `shouldBe` (BoundedSet (0, 0) (5, 10) (mkSet [(0, 0), (2, 7), (5, 10)]))
    it "mirrors BoundedSet H" $ toOrigin ((BoundedSet @(Integer, Integer) (0, 0) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ◐) `shouldBe` (BoundedSet (0, 0) (10, 5) (mkSet [(10, 0), (3, 2), (0, 5)]))
    it "mirrors BoundedSet V" $ toOrigin ((BoundedSet @(Integer, Integer) (0, 0) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ◓) `shouldBe` (BoundedSet (0, 0) (10, 5) (mkSet [(0, 5), (7, 3), (10, 0)]))
    it "rotates BoundedSet CW" $ toOrigin ((BoundedSet @(Integer, Integer) (0, 0) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ↻) `shouldBe` (BoundedSet (0, 0) (5, 10) (mkSet [(0, 10), (2, 3), (5, 0)]))
    it "rotates BoundedSet CCW" $ toOrigin ((BoundedSet @(Integer, Integer) (0, 0) (10, 5) (mkSet [(0, 0), (7, 2), (10, 5)])) ↺) `shouldBe` (BoundedSet (0, 0) (5, 10) (mkSet [(5, 0), (3, 7), (0, 10)]))

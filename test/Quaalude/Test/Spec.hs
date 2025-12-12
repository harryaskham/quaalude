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

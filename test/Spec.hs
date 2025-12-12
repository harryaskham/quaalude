module Spec where

import Quaalude.Test.Spec qualified as QuaaludeSpec
import Test.Hspec

main :: IO ()
main = hspec QuaaludeSpec.spec

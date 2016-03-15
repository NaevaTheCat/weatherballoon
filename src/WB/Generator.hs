module WB.Generator where

import Control.Monad
import Test.QuickCheck

import WB.Types

class Variant a where
  valid :: Gen a
  invalid :: Gen a

instance Variant a => Arbitrary a where
  arbitrary = oneof [valid, invalid]


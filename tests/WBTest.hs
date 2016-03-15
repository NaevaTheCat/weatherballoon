import Control.Monad
import Test.QuickCheck

import WB.Types
import WB.Parse

prop_idempotent r = (parseR $ printR r) == Just r

main :: IO ()
main = quickCheck suite

suite = do
  prop_idempotent

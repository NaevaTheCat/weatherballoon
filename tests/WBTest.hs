import Control.Monad
import Test.QuickCheck

import Data.Text (Text)
import qualified Data.Text as T


import WB.Types
import WB.Parse

prop_idempotent r = (parseR $ printR r) == Just r
prop_rejects r = (parseR r) == Nothing --vanishingly small chance we'll generate a valid input

main :: IO ()
main = do
  quickCheck prop_idempotent
  quickCheck prop_rejects
  



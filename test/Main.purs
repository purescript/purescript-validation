module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Parallel (parallel)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Validation.Semigroup (V, unV)
import Test.Assert (ASSERT, assert)

invalid :: forall a. Either String a
invalid = Left "Fail"

valid :: forall e. Either e Unit
valid = Right unit

applicativeify :: forall e a. Either e a -> V (Array e) a
applicativeify = parallel <<< lmap pure

pseudoParser :: V (Array String) Unit
pseudoParser
  = applicativeify valid
  *> applicativeify invalid
  *> applicativeify valid
  *> applicativeify invalid
  *> applicativeify invalid

main :: Eff (assert :: ASSERT) Unit
main = do
  unV (checkErrors 3) (const (pure unit)) pseudoParser
  where
  checkErrors n errs = assert (A.length errs == n)

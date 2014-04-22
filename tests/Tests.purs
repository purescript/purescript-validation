module Main where

import Debug.Trace

import Control.Monad.Eff

import Data.Validation

foreign import throw 
  "function $$throw(msg) {\
  \  throw msg;\
  \}" :: forall a. String -> a

assert :: forall eff. String -> Boolean -> Eff eff {}
assert _ true = return {}
assert msg false = throw ("Test failed: " ++ msg)

assertInvalid :: forall err result. (Show err, Eq err) => err -> String -> V err result -> Eff (trace :: Trace) {}
assertInvalid expectedError msg = assert msg <<< runV ((==) expectedError) (const false)

data Test a b = Test a b

main = do
  assert "isValid returns true on pure values" $ isValid (pure "Testing" :: V String String) 
  assert "isValid returns false on invalid values" $ not <<< isValid $ invalid "Error"

  assertInvalid "Foo" "<*> propagates errors from the left" $ Test <$> invalid "Foo" <*> pure "Bar"
  assertInvalid "Bar" "<*> propagates errors from the right" $ Test <$> pure "Foo" <*> invalid "Bar"
  assertInvalid "FooBar" "<*> accumulates errors" $ Test <$> invalid "Foo" <*> invalid "Bar"
  assert "<*> combines pure values" $ isValid $ (Test <$> pure "Foo" <*> pure "Bar") :: V String (Test String String)

  trace "Done"

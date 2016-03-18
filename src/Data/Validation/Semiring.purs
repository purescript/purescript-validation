-- | This module defines a variant of applicative validation with
-- | an `Alternative` instance, for validators which support errors
-- | with multiple alternatives.
-- |
-- | The API is equivalent to `Data.Validation`,
-- | but uses `Semiring` instead of `Semigroup`.
module Data.Validation.Semiring
  ( V
  , unV
  , invalid
  , isValid
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Control.Alternative (class Alternative)

-- | The `V` functor, used for alternative validation
-- |
-- | The `Alternative` instance collects multiple failures in
-- | an arbitrary `Semiring`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | import Data.Semiring.Free
-- |
-- | validate r :: Person -> V (Free Error) Person
-- | validate person = { first: _, last: _, contact: _}
-- |   <$> validateName person.first
-- |   <*> validateName person.last
-- |   <*> (validateEmail person.contact <|> validatePhone person.contact)
-- | ```
data V err res = Valid res | Invalid err

-- | Unpack the `V` type constructor, providing functions to handle the error
-- | and success cases.
unV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
unV f _ (Invalid err) = f err
unV _ g (Valid result) = g result

-- | Fail with a validation error
invalid :: forall err result. err -> V err result
invalid = Invalid

-- | Test whether validation was successful or not
isValid :: forall err result. V err result -> Boolean
isValid (Valid _) = true
isValid _ = false

instance eqV :: (Eq err, Eq result) => Eq (V err result) where
  eq (Invalid err1) (Invalid err2) = err1 == err2
  eq (Valid result1) (Valid result2) = result1 == result2
  eq _ _ = false

instance ordV :: (Ord err, Ord result) => Ord (V err result) where
  compare (Invalid err1) (Invalid err2) = compare err1 err2
  compare (Invalid _) _ = LT
  compare (Valid result1) (Valid result2) = compare result1 result2
  compare (Valid _) _ = GT

instance showV :: (Show err, Show result) => Show (V err result) where
  show (Invalid err) = "Invalid (" <> show err <> ")"
  show (Valid result) = "Valid (" <> show result <> ")"

instance functorV :: Functor (V err)  where
  map _ (Invalid err) = Invalid err
  map f (Valid result) = Valid (f result)

instance applyV :: (Semiring err) => Apply (V err)  where
  apply (Invalid err1) (Invalid err2) = Invalid (err1 * err2)
  apply (Invalid err) _ = Invalid err
  apply _ (Invalid err) = Invalid err
  apply (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semiring err) => Applicative (V err) where
  pure = Valid

instance altV :: (Semiring err) => Alt (V err) where
  alt (Invalid err1) (Invalid err2) = Invalid (err1 + err2)
  alt (Invalid _) a = a
  alt (Valid a) _ = Valid a

instance plusV :: (Semiring err) => Plus (V err) where
  empty = Invalid zero

instance alernativeV :: (Semiring err) => Alternative (V err)

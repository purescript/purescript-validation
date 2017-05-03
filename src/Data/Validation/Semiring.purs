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

import Control.Apply (lift2)
import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Control.Alternative (class Alternative)

import Data.Bifunctor (class Bifunctor)
import Data.Monoid (class Monoid, mempty)

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

derive instance eqV :: (Eq err, Eq result) => Eq (V err result)

derive instance ordV :: (Ord err, Ord result) => Ord (V err result)

instance showV :: (Show err, Show result) => Show (V err result) where
  show (Invalid err) = "Invalid (" <> show err <> ")"
  show (Valid result) = "Valid (" <> show result <> ")"

instance functorV :: Functor (V err)  where
  map _ (Invalid err) = Invalid err
  map f (Valid result) = Valid (f result)

instance bifunctorV :: Bifunctor V where
  bimap f _ (Invalid err) = Invalid (f err)
  bimap _ g (Valid result) = Valid (g result)

instance applyV :: (Semiring err) => Apply (V err)  where
  apply (Invalid err1) (Invalid err2) = Invalid (err1 * err2)
  apply (Invalid err) _ = Invalid err
  apply _ (Invalid err) = Invalid err
  apply (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semiring err) => Applicative (V err) where
  pure = Valid

instance semigroupV :: (Semiring err, Semigroup a) => Semigroup (V err a) where
  append = lift2 append

instance monoidV :: (Semiring err, Monoid a) => Monoid (V err a) where
  mempty = pure mempty

instance altV :: (Semiring err) => Alt (V err) where
  alt (Invalid err1) (Invalid err2) = Invalid (err1 + err2)
  alt (Invalid _) a = a
  alt (Valid a) _ = Valid a

instance plusV :: (Semiring err) => Plus (V err) where
  empty = Invalid zero

instance alernativeV :: (Semiring err) => Alternative (V err)

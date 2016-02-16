-- | This module defines an applicative functor for _applicative validation_.
-- |
-- | Applicative validation differs from monadic validation using `Either` in
-- | that it allows us to collect multiple errors using a `Semigroup`, whereas
-- | `Either` terminates on the first error.

module Data.Validation (
  V(),
  invalid,
  runV,
  isValid
  ) where

import Prelude (class Applicative, class Semigroup, class Apply, class Functor, class Show, (<>), show, (++))

-- | The `V` functor, used for applicative validation
-- |
-- | The `Applicative` instance collects multiple failures in
-- | an arbitrary `Semigroup`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | validate :: Person -> V (Array Error) Person
-- | validate person = { first: _, last: _, email: _ }
-- |   <$> validateName person.first
-- |   <*> validateName person.last
-- |   <*> validateEmail person.email
-- | ```
data V err result = Invalid err | Valid result

-- | Fail with a validation error
invalid :: forall err result. err -> V err result
invalid = Invalid

-- | Unpack the `V` type constructor, providing functions to handle the error
-- | and success cases.
runV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
runV f _ (Invalid err) = f err
runV _ g (Valid result) = g result

-- | Test whether validation was successful or not
isValid :: forall err result. V err result -> Boolean
isValid (Valid _) = true
isValid _ = false

instance showV :: (Show err, Show result) => Show (V err result) where
  show (Invalid err) = "Invalid (" ++ show err ++ ")"
  show (Valid result) = "Valid (" ++ show result ++ ")"

instance functorV :: Functor (V err) where
  map _ (Invalid err) = Invalid err
  map f (Valid result) = Valid (f result)

instance applyV :: (Semigroup err) => Apply (V err) where
  apply (Invalid err1) (Invalid err2) = Invalid (err1 <> err2)
  apply (Invalid err) _ = Invalid err
  apply _ (Invalid err) = Invalid err
  apply (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semigroup err) => Applicative (V err) where
  pure = Valid

-- | This module defines an applicative functor for _applicative validation_.
-- |
-- | Applicative validation differs from monadic validation using `Either` in
-- | that it allows us to collect multiple errors using a `Semigroup`, whereas
-- | `Either` terminates on the first error.

module Data.Validation.Semigroup
  ( V(..)
  , validation
  , invalid
  , isValid
  , toEither
  , andThen
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.Newtype (class Newtype)

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
newtype V err result = V (Either err result)

derive instance newtypeV :: Newtype (V err result) _

-- | Takes two functions an a `V` value, if the validation failed the error is
-- | applied to the first function, if the validation succeeded the inner value
-- | is applied to the second function.
validation :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
validation f _ (V (Left err)) = f err
validation _ g (V (Right result)) = g result

-- | Fail with a validation error.
invalid :: forall err result. err -> V err result
invalid = V <<< Left

-- | Test whether validation was successful or not.
isValid :: forall err result. V err result -> Boolean
isValid (V (Right _)) = true
isValid _ = false

toEither :: forall err result. V err result -> Either err result
toEither (V e) = e

-- | Apply a function if successful, to enable chaining of validation.
-- |
-- | Similar to a monadic bind, except it is inconsistent with Apply - that is,
-- | where as apply accumulates failures: `apply (invalid x) (invalid y) = invalid (x <> y)`,
-- | andThen has fail-fast semantics: `andThen (invalid x) (\_ -> invalid y) = invalid x`
-- | (`>>=` would be expected to be consistent).
andThen :: forall err a b. V err a -> (a -> V err b) -> V err b
andThen v1 f =
  validation invalid f v1

derive instance eqV :: (Eq err, Eq result) => Eq (V err result)
derive instance eq1V :: Eq err => Eq1 (V err)

derive instance ordV :: (Ord err, Ord result) => Ord (V err result)
derive instance ord1V :: Ord err => Ord1 (V err)

instance showV :: (Show err, Show result) => Show (V err result) where
  show = case _ of
    V (Left err) -> "invalid (" <> show err <> ")"
    V (Right result) -> "pure (" <> show result <> ")"

derive newtype instance functorV :: Functor (V err)

derive newtype instance bifunctorV :: Bifunctor V

instance applyV :: Semigroup err => Apply (V err) where
  apply (V (Left err1)) (V (Left err2)) = V (Left (err1 <> err2))
  apply (V (Left err)) _ = V (Left err)
  apply _ (V (Left err)) = V (Left err)
  apply (V (Right f)) (V (Right x)) = V (Right (f x))

instance applicativeV :: Semigroup err => Applicative (V err) where
  pure = V <<< Right

instance semigroupV :: (Semigroup err, Semigroup a) => Semigroup (V err a) where
  append = lift2 append

instance monoidV :: (Semigroup err, Monoid a) => Monoid (V err a) where
  mempty = pure mempty

instance foldableV :: Foldable (V err) where
  foldMap = validation (const mempty)
  foldr f b = validation (const b) (flip f b)
  foldl f b = validation (const b) (f b)

instance traversableV :: Traversable (V err) where
  sequence = validation (pure <<< V <<< Left) (map (V <<< Right))
  traverse f = validation (pure <<< V <<< Left) (map (V <<< Right) <<< f)

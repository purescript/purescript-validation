-- | This module defines a variant of applicative validation with
-- | an `Alt` instance, for validators which support errors
-- | with multiple alternatives.
module Data.Validation.Semiring
  ( V(..)
  , unV
  , invalid
  , isValid
  , toEither
  , andThen
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Plus (class Plus)
import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)
import Data.Newtype (class Newtype)

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
-- |
-- | main :: Effect Unit
-- | main = do
-- |   logShow $ validate { first: "Jonh", last: "Doe", contact: "invalidEmailOrPhone" }
-- |   -- for example if all validators return error, it will output
-- |   -- > invalid ((Free (((InvalidName "Jonh") : (InvalidName "Doe") : (InvalidEmail "invalidEmailOrPhone") : Nil) : ((InvalidName "Jonh") : (InvalidName "Doe") : (InvalidPhone "invalidEmailOrPhone") : Nil) : Nil)))
-- | ```
newtype V err result = V (Either err result)

derive instance newtypeV :: Newtype (V err result) _

-- | Unpack the `V` type constructor, providing functions to handle the error
-- | and success cases.
unV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
unV f _ (V (Left err)) = f err
unV _ g (V (Right result)) = g result

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
-- | where as `apply (V err a) (V err a)` accumulates failures,
-- | `(V err a) ``andThen`` (\a -> V err a)` has fail-fast semantics
-- | (`>>=` would be expected to be consistent).
andThen :: forall err a b. V err a -> (a -> V err b) -> V err b
andThen v1 f =
  unV invalid f v1

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

instance applyV :: Semiring err => Apply (V err)  where
  apply (V (Left err1)) (V (Left err2)) = V (Left (err1 * err2))
  apply (V (Left err)) _ = V (Left err)
  apply _ (V (Left err)) = V (Left err)
  apply (V (Right f)) (V (Right x)) = V (Right (f x))

instance applicativeV :: Semiring err => Applicative (V err) where
  pure = V <<< Right

instance semigroupV :: (Semiring err, Semigroup a) => Semigroup (V err a) where
  append = lift2 append

instance monoidV :: (Semiring err, Monoid a) => Monoid (V err a) where
  mempty = pure mempty

instance altV :: Semiring err => Alt (V err) where
  alt (V (Left err1)) (V (Left err2)) = V (Left (err1 + err2))
  alt (V (Left _)) a = a
  alt (V (Right a)) _ = V (Right a)

instance plusV :: Semiring err => Plus (V err) where
  empty = V (Left zero)

instance foldableV :: Foldable (V err) where
  foldMap = unV (const mempty)
  foldr f b = unV (const b) (flip f b)
  foldl f b = unV (const b) (f b)

instance traversableV :: Traversable (V err) where
  sequence = unV (pure <<< V <<< Left) (map (V <<< Right))
  traverse f = unV (pure <<< V <<< Left) (map (V <<< Right) <<< f)

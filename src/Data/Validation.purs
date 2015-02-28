module Data.Validation
  ( Validation()
  , invalid
  , runValidation
  , isValid
  ) where

data Validation err result = Invalid err | Valid result

invalid :: forall err result. err -> Validation err result
invalid = Invalid

runValidation :: forall err result r. (err -> r) -> (result -> r) -> Validation err result -> r
runValidation f _ (Invalid err) = f err
runValidation _ g (Valid result) = g result

isValid :: forall err result r. Validation err result -> Boolean
isValid (Valid _) = true
isValid _ = false

instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show (Invalid err) = "Invalid (" ++ show err ++ ")"
  show (Valid result) = "Valid (" ++ show result ++ ")"

instance functorValidation :: Functor (Validation err) where
  (<$>) _ (Invalid err) = Invalid err
  (<$>) f (Valid result) = Valid (f result)

instance applyValidation :: (Semigroup err) => Apply (Validation err) where
  (<*>) (Invalid err1) (Invalid err2) = Invalid (err1 <> err2)
  (<*>) (Invalid err) _ = Invalid err
  (<*>) _ (Invalid err) = Invalid err
  (<*>) (Valid f) (Valid x) = Valid (f x)

instance applicativeValidation :: (Semigroup err) => Applicative (Validation err) where
  pure = Valid

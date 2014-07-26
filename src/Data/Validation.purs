module Data.Validation (
  V(),
  invalid,
  runV,
  isValid
  ) where

data V err result = Invalid err | Valid result

invalid :: forall err result. err -> V err result
invalid = Invalid

runV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
runV f _ (Invalid err) = f err
runV _ g (Valid result) = g result

isValid :: forall err result r. V err result -> Boolean
isValid (Valid _) = true
isValid _ = false

instance showV :: (Show err, Show result) => Show (V err result) where
  show (Invalid err) = "Invalid (" ++ show err ++ ")"
  show (Valid result) = "Valid (" ++ show result ++ ")"

instance functorV :: Functor (V err) where
  (<$>) _ (Invalid err) = Invalid err
  (<$>) f (Valid result) = Valid (f result)

instance applyV :: (Semigroup err) => Apply (V err) where
  (<*>) (Invalid err1) (Invalid err2) = Invalid (err1 <> err2)
  (<*>) (Invalid err) _ = Invalid err
  (<*>) _ (Invalid err) = Invalid err
  (<*>) (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semigroup err) => Applicative (V err) where
  pure = Valid


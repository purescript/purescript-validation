-- | This module defines an applicative functor and alt instance for 
-- | validation that can go multiple ways
-- |
-- | This validation works exactly as `Data.Validation.Semigroup`
-- | but uses `Semiring` instead of `Semigroup`
module Data.Validation.Semiring (
  V(),
  invalid,
  runV,
  isValid
  ) where

import Control.Alt


-- | example
-- | ```purescript
-- | validate r :: Person -> V (FreeSemiring Error) Person 
-- | validate person = { first: _, last: _, contact: _}
-- |   <$> validateName person.first
-- |   <*> validateName person.last
-- |   <*> (validateEmail person.contact <|> validatePhone person.contact)
-- | ```
data V err res = Valid res | Invalid err

-- | Fail 
invalid :: forall err result. err -> V err result
invalid = Invalid

-- | Unpack
runV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
runV f _ (Invalid err) = f err
runV _ g (Valid result) = g result

-- | Check if valid
isValid :: forall err result. V err result -> Boolean
isValid (Valid _) = true
isValid _ = false

instance showV :: (Show err, Show result) => Show (V err result) where
  show (Invalid err) = "Invalid (" ++ show err ++ ")"
  show (Valid result) = "Valid (" ++ show result ++ ")"

instance functorV :: Functor (V err)  where
  (<$>) _ (Invalid err) = Invalid err
  (<$>) f (Valid result) = Valid (f result)

instance applyV :: (Semiring err) => Apply (V err)  where
  (<*>) (Invalid err1) (Invalid err2) = Invalid (err1 * err2)
  (<*>) (Invalid err) _ = Invalid err
  (<*>) _ (Invalid err) = Invalid err
  (<*>) (Valid f) (Valid x) = Valid (f x)

instance applicativeV :: (Semiring err) => Applicative (V err) where
  pure = Valid

instance altV :: (Semiring err) => Alt (V err) where
  (<|>) (Invalid err1) (Invalid err2) = Invalid (err1 + err2)
  (<|>) (Invalid _) a = a
  (<|>) (Valid a) _ = Valid a 

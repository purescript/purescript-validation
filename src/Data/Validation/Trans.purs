module Data.Validation.Trans
  ( ValidationT(..)
  , runValidationT
  ) where

import Control.Apply (lift2)
import Control.Monad.Trans
import Data.Validation

newtype ValidationT err m a = ValidationT (m (Validation err a))

runValidationT :: forall m err a. (Semigroup err, Monad m) => ValidationT err m a -> m (Validation err a)
runValidationT (ValidationT v) = v

instance monadTransValidationT :: (Semigroup err) => MonadTrans (ValidationT err) where
  lift m = ValidationT $ m >>= return <<< pure

instance applyValidationT :: (Semigroup err, Apply m) => Apply (ValidationT err m) where
  (<*>) (ValidationT f) (ValidationT a) = ValidationT (lift2 (<*>) f a)

instance applicativeValidationT :: (Semigroup err, Monad m) => Applicative (ValidationT err m) where
  pure = ValidationT <<< return <<< pure

instance functorValidationT :: (Functor m) => Functor (ValidationT err m) where
  (<$>) f (ValidationT m) = ValidationT $ (f <$>) <$> m

instance bindValidationT :: (Semigroup err, Monad m) => Bind (ValidationT err m) where
  (>>=) (ValidationT m) f = ValidationT $ m >>= runValidationT <<< runValidation liftErr f
    where liftErr = ValidationT <<< return <<< invalid

instance monadValidationT :: (Semigroup err, Monad m) => Monad (ValidationT err m)

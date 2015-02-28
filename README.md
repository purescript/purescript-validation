# Module Documentation

## Module Data.Validation

#### `Validation`

``` purescript
data Validation err result
```


#### `invalid`

``` purescript
invalid :: forall err result. err -> Validation err result
```


#### `runValidation`

``` purescript
runValidation :: forall err result r. (err -> r) -> (result -> r) -> Validation err result -> r
```


#### `isValid`

``` purescript
isValid :: forall err result r. Validation err result -> Boolean
```


#### `showValidation`

``` purescript
instance showValidation :: (Show err, Show result) => Show (Validation err result)
```


#### `functorValidation`

``` purescript
instance functorValidation :: Functor (Validation err)
```


#### `applyValidation`

``` purescript
instance applyValidation :: (Semigroup err) => Apply (Validation err)
```


#### `applicativeValidation`

``` purescript
instance applicativeValidation :: (Semigroup err) => Applicative (Validation err)
```



## Module Data.Validation.Trans

#### `ValidationT`

``` purescript
newtype ValidationT err m a
  = ValidationT (m (Validation err a))
```


#### `runValidationT`

``` purescript
runValidationT :: forall m err a. (Semigroup err, Monad m) => ValidationT err m a -> m (Validation err a)
```


#### `monadTransValidationT`

``` purescript
instance monadTransValidationT :: (Semigroup err) => MonadTrans (ValidationT err)
```


#### `applyValidationT`

``` purescript
instance applyValidationT :: (Semigroup err, Apply m) => Apply (ValidationT err m)
```


#### `applicativeValidationT`

``` purescript
instance applicativeValidationT :: (Semigroup err, Monad m) => Applicative (ValidationT err m)
```


#### `functorValidationT`

``` purescript
instance functorValidationT :: (Functor m) => Functor (ValidationT err m)
```


#### `bindValidationT`

``` purescript
instance bindValidationT :: (Semigroup err, Monad m) => Bind (ValidationT err m)
```


#### `monadValidationT`

``` purescript
instance monadValidationT :: (Semigroup err, Monad m) => Monad (ValidationT err m)
```





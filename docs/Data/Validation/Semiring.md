## Module Data.Validation.Semiring

This module defines a variant of applicative validation with 
an `Alternative` instance, for validators which support errors
with multiple alternatives.

The API is equivalent to `Data.Validation`,
but uses `Semiring` instead of `Semigroup`.

#### `V`

``` purescript
data V err res
```

The `V` functor, used for alternative validation

The `Alternative` instance collects multiple failures in
an arbitrary `Semiring`.

For example:

```purescript
import Data.Semiring.Free

validate r :: Person -> V (Free Error) Person 
validate person = { first: _, last: _, contact: _}
  <$> validateName person.first
  <*> validateName person.last
  <*> (validateEmail person.contact <|> validatePhone person.contact)
```

##### Instances
``` purescript
instance showV :: (Show err, Show result) => Show (V err result)
instance functorV :: Functor (V err)
instance applyV :: (Semiring err) => Apply (V err)
instance applicativeV :: (Semiring err) => Applicative (V err)
instance altV :: (Semiring err) => Alt (V err)
instance plusV :: (Semiring err) => Plus (V err)
instance alernativeV :: (Semiring err) => Alternative (V err)
```

#### `invalid`

``` purescript
invalid :: forall err result. err -> V err result
```

Fail with a validation error

#### `runV`

``` purescript
runV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
```

Unpack the `V` type constructor, providing functions to handle the error
and success cases.

#### `isValid`

``` purescript
isValid :: forall err result. V err result -> Boolean
```

Test whether validation was successful or not



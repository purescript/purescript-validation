# Module Documentation

## Module Data.Validation


This module defines an applicative functor for _applicative validation_.

Applicative validation differs from monadic validation using `Either` in
that it allows us to collect multiple errors using a `Semigroup`, whereas
`Either` terminates on the first error.

#### `V`

``` purescript
data V err result
```

The `V` functor, used for applicative validation

The `Applicative` instance collects multiple failures in
an arbitrary `Semigroup`.

For example:

```purescript
validate :: Person -> V [Error] Person
validate person = { first: _, last: _, email: _ }
  <$> validateName person.first
  <*> validateName person.last
  <*> validateEmail person.email
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
isValid :: forall err result r. V err result -> Boolean
```

Test whether validation was successful or not

#### `showV`

``` purescript
instance showV :: (Show err, Show result) => Show (V err result)
```


#### `functorV`

``` purescript
instance functorV :: Functor (V err)
```


#### `applyV`

``` purescript
instance applyV :: (Semigroup err) => Apply (V err)
```


#### `applicativeV`

``` purescript
instance applicativeV :: (Semigroup err) => Applicative (V err)
```
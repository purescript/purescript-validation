# Module Documentation

## Module Data.Validation

### Types

    data V err result where


### Type Class Instances

    instance applicativeV :: (Semigroup err) => Applicative (V err)

    instance applyV :: (Semigroup err) => Apply (V err)

    instance functorV :: Functor (V err)


### Values

    invalid :: forall err result. err -> V err result

    isValid :: forall err result r. V err result -> Prim.Boolean

    runV :: forall err result r. (err -> r) -> (result -> r) -> V err result -> r
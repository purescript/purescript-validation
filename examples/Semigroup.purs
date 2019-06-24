module Semigroup where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, over2)
import Data.String (length, null, toLower, toUpper)
import Data.Validation.Semigroup (V, invalid)
import Global.Unsafe (unsafeStringify)

-- | `UnvalidatedFormData` represents the raw data we might receive from a form
-- | before any validation has been performed.
-- |
-- | Note that both the `username` and `password` fields in this record are
-- | simple `String` types.
type UnvalidatedFormData =
  { username :: String
  , password :: String
  }

-- | `Username` is a wrapper around `String` that allows us to distinguish a
-- | field containing a valid username from any other potential `String`s.
newtype Username = Username String

-- | `Password` is a wrapper around `String` that allows us to distinguish a
-- | field containing a valid password from any other potential `String`s.
newtype Password = Password String

-- | `ValidatedFormData` represents the valid data from a form that is produced
-- | as a result of our validation process.
-- |
-- | Note that the `username` and `password` fields that were simple `String`s
-- | in `UnvalidatedFormData` are now `Username` and `Password`, respectively.
type ValidatedFormData =
  { username :: Username
  , password :: Password
  }

-- | `ValidationError` represents the potential errors we might encounter during
-- | the validation process.
data ValidationError
  = FieldIsEmpty
  | FieldIsTooShort
  | FieldIsAllLower
  | FieldIsAllUpper

-- | Generically derive a `Show` instance for `ValidationError` so that we may
-- | print these errors to the console later.
derive instance genericValidationError :: Generic ValidationError _
instance showValidationError :: Show ValidationError where
  show = genericShow

-- | A note on `Data.Validation.Semigroup`'s `V`:
-- |
-- | `V` is a sum type with an `Invalid` side that collects the errors
-- | encountered during the validation process, and a `Valid` side that holds
-- | the result of the successful validation.

-- | This function validates that an input `String` is not empty.
-- |
-- | If the input is empty, it returns a `FieldIsEmpty` error on the `Invalid`
-- | side of `V`.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateNonEmpty :: String -> V (NonEmptyArray ValidationError) String
validateNonEmpty input
  | null input = invalid $ NonEmpty.singleton FieldIsEmpty
  | otherwise = pure input

-- | This function validates that an input `String` is at greater than or equal
-- | to the given `validLength`.
-- |
-- | If the input is less than `validLength` characters long, it returns a
-- | `FieldIsTooShort` error on the `Invalid` side of `V`.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateMinimumLength :: Int -> String -> V (NonEmptyArray ValidationError) String
validateMinimumLength validLength input
  | length input <= validLength = invalid (NonEmpty.singleton FieldIsTooShort)
  | otherwise = pure input

-- | This function validates that an input `String` uses some mix of upper- and
-- | lower-case characters (i.e. is mixed case).
-- |
-- | If the input isn't mixed case, it returns a `FieldIsAllUpper` or
-- | `FieldIsAllLower` error on the `Invalid` side of `V`, depending on whether
-- | the field was entirely upper- or lower-case, respectively.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateMixedCase :: String -> V (NonEmptyArray ValidationError) String
validateMixedCase input
  | toLower input == input = invalid (NonEmpty.singleton FieldIsAllLower)
  | toUpper input == input = invalid (NonEmpty.singleton FieldIsAllUpper)
  | otherwise = pure input

-- | `InvalidField` represents the fields of some form that have failed 
-- | validation
-- |
-- | It is used as a key for the `Map` that associates `NonEmptyArray`s of
-- | `ValidationError`s with the field that was invalid.
data InvalidField
  = InvalidUsername
  | InvalidPassword

-- | Generically derive a `Show` instance for `InvalidField` so that we may
-- | print these errors to the console later.
derive instance genericInvalidField :: Generic InvalidField _
instance showInvalidField :: Show InvalidField where
  show = genericShow

-- | Generically derive an `Eq` instance for `InvalidField` so that we may
-- | generically derive an `Ord` instance, so that it may be used as a key in a
-- | `Map`.
instance eqInvalidField :: Eq InvalidField where
  eq = genericEq

-- | Generically derive an `Ord` instance for `InvalidField` so that we may
-- | use it as a key in a `Map`.
instance ordInvalidField :: Ord InvalidField where
  compare = genericCompare

-- | `FormValidationErrors` represents all `ValidationError`s associated with
-- | a particular `ValidationField` that was invalid.
newtype FormValidationErrors =
  FormValidationErrors (Map InvalidField (NonEmptyArray ValidationError))

-- | Derive a `Newtype` isntance for `FormValidationErrors` so that we may use
-- | generic functions that can operate over it as if it were a plain 
-- | `Map InvalidField (NonEmptyArray ValidationError)`.
derive instance newtypeFormValidationErrors :: Newtype FormValidationErrors _

-- | Derive a `Semigroup` instance for `FormValidationErrors` that combines
-- | errors using the `Map.unionWith` operation, so as to avoid returning 
-- | duplicate entries when fields fail with overlapping errors.
instance semigroupFormValidationErrors :: Semigroup FormValidationErrors where
  append = over2 FormValidationErrors (Map.unionWith (<>))

-- | Generically derive a `Show` instance for `FormValidationError` so that we
-- | may print these errors to the console later.
derive instance genericFormValidationError :: Generic FormValidationErrors _
instance showFormValidationErrors :: Show FormValidationErrors where
  show = genericShow

-- | This function validates that an input string conforms to our requirements
-- | for a valid username. Namely, we require that the input be non-empty and at
-- | least 4 characters long.
-- |
-- | If the input doesn't conform to these requirements, the failures
-- | encountered during validation will be collected on the `Invalid` side of
-- | `V`, tagged with a `BadUsername` to identify the part of the form that
-- | failed validation, and wrapped in an `Array` so that additional errors may
-- | be collected along with it.
-- |
-- | Otherwise, it returns the input wrapped in the `Username` newtype to
-- | distinguish it from a normal, unvalidated `String`.
validateUsername :: String -> V FormValidationErrors Username
validateUsername input = 
  bimap (FormValidationErrors <<< Map.singleton InvalidUsername) Username
    $  validateNonEmpty input
    *> validateMinimumLength 4 input

-- | This function validates that an input string conforms to our requirements
-- | for a valid password. Namely, we require that the input be non-empty, at
-- | least 6 characters long, and contains both upper- and lower-case
-- | characters.
-- |
-- | If the input doesn't conform to these requirements, the failures
-- | encountered during validation will be collected on the `Invalid` side of
-- | `V`, tagged with a `BadPassword` to identify the part of the form that
-- | failed validation, and wrapped in an `Array` so that additional errors may
-- | be collected along with it.
-- |
-- | Otherwise, it returns the input wrapped in the `Password` newtype to
-- | distinguish it from a normal, unvalidated `String`.
validatePassword :: String -> V FormValidationErrors Password
validatePassword input = 
  bimap (FormValidationErrors <<< Map.singleton InvalidPassword) Password
    $  validateNonEmpty input
    *> validateMinimumLength 6 input
    *> validateMixedCase input

-- | This function validates that an `UnvalidatedFormData` record contains both
-- | a valid username and a valid password, per the requirements specified in
-- | our `validateUsername` and `validatePassword` functions above.
-- |
-- | If the form doesn't conform to these requirements, the failures encountered
-- | during any and all of the validation steps above will be collected on the
-- | `Invalid` side of `V`.
-- |
-- | Otherwise, it returns the validated fields in the `ValidatedFormData`
-- | record specified above.
validateForm 
  :: UnvalidatedFormData 
  -> V FormValidationErrors ValidatedFormData
validateForm {username, password} = {username: _, password: _}
  <$> validateUsername username
  <*> validatePassword password

-- | This is a form that will fail validation, since both fields are empty
-- | strings.
emptyUsernameAndPassword :: UnvalidatedFormData
emptyUsernameAndPassword = {username: "", password: ""}

-- | This is a form that will fail validation, since both fields are too short.
shortUsernameAndPassword :: UnvalidatedFormData
shortUsernameAndPassword = {username: "foo", password: "bar"}

-- | This is a form that will fail validation, since the password lowercase.
lowerCasePassword :: UnvalidatedFormData
lowerCasePassword = {username: "alice", password: "foobarbaz"}

-- | This is a form that will fail validation, since the password uppercase.
upperCasePassword :: UnvalidatedFormData
upperCasePassword = {username: "alice", password: "FOOBARBAZ"}

-- | This is a form that will pass validation, as it conforms to all the
-- | requirements outlined in the validation functions above.
goodForm :: UnvalidatedFormData
goodForm = {username: "alice", password: "FooBarBaz"}

-- | Run through all of the example forms and print the validation results to
-- | the console.
-- |
-- | We'll cheat a little here and use `unsafeStringify` to get a `Show`able
-- | version of our `ValidatedFormData` record.
main :: Effect Unit
main = do
  printValidation $ validateForm emptyUsernameAndPassword
  -- > invalid ((FormValidationErrors (fromFoldable [(Tuple InvalidUsername (NonEmptyArray [FieldIsEmpty,FieldIsTooShort])),(Tuple InvalidPassword (NonEmptyArray [FieldIsEmpty,FieldIsTooShort,FieldIsAllLower]))])))

  printValidation $ validateForm shortUsernameAndPassword
  -- > invalid ((FormValidationErrors (fromFoldable [(Tuple InvalidUsername (NonEmptyArray [FieldIsTooShort])),(Tuple InvalidPassword (NonEmptyArray [FieldIsTooShort,FieldIsAllLower]))])))

  printValidation $ validateForm lowerCasePassword
  -- > invalid ((FormValidationErrors (fromFoldable [(Tuple InvalidPassword (NonEmptyArray [FieldIsAllLower]))])))

  printValidation $ validateForm upperCasePassword
  -- > invalid ((FormValidationErrors (fromFoldable [(Tuple InvalidPassword (NonEmptyArray [FieldIsAllUpper]))])))

  printValidation $ validateForm goodForm
  -- > pure ("{\"username\":\"alice\",\"password\":\"FooBarBaz\"}")

  where
    printValidation = logShow <<< (map unsafeStringify)

module Semigroup where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Data.Array (singleton)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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

-- | `ValidationErrors` is a helpful type alias for an `Array` of the errors
-- | we might encounter during the validation process.
type ValidationErrors = Array ValidationError

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
validateNonEmpty :: String -> V ValidationErrors String
validateNonEmpty input
  | null input = invalid [FieldIsEmpty]
  | otherwise = pure input

-- | This function validates that an input `String` is at greater than or equal
-- | to the given `validLength`.
-- |
-- | If the input is less than `validLength` characters long, it returns a
-- | `FieldIsTooShort` error on the `Invalid` side of `V`.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateMinimumLength :: Int -> String -> V ValidationErrors String
validateMinimumLength validLength input
  | length input <= validLength = invalid [FieldIsTooShort]
  | otherwise = pure input

-- | This function validates that an input `String` uses some mix of upper- and
-- | lower-case characters (i.e. is mixed case).
-- |
-- | If the input isn't mixed case, it returns a `FieldIsAllUpper` or
-- | `FieldIsAllLower` error on the `Invalid` side of `V`, depending on whether
-- | the field was entirely upper- or lower-case, respectively.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateMixedCase :: String -> V ValidationErrors String
validateMixedCase input
  | toLower input == input = invalid [FieldIsAllLower]
  | toUpper input == input = invalid [FieldIsAllUpper]
  | otherwise = pure input

-- | `FormValidationError` represents the errors we might encounter while
-- | attempting to validate the username and password fields of our form.
-- |
-- | The `BadUsername` and `BadPassword` branches help us distinguish which
-- | part of the form failed validation.
data FormValidationError
  = BadUsername ValidationErrors
  | BadPassword ValidationErrors

-- | Generically derive a `Show` instance for `FormValidationError` so that we
-- | may print these errors to the console later.
derive instance genericFormValidationError :: Generic FormValidationError _
instance showFormValidationError :: Show FormValidationError where
  show = genericShow

-- | Much like `ValidationErrors`, `FormValidationErrors` is a helpful alias
-- | for an `Array` of errors specific to the validation of our form fields.
type FormValidationErrors = Array FormValidationError

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
validateUsername input = bimap (singleton <<< BadUsername) Username
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
validatePassword input = bimap (singleton <<< BadPassword) Password
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
validateForm :: UnvalidatedFormData -> V FormValidationErrors ValidatedFormData
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
  -- > (Invalid [(BadUsername [FieldIsEmpty,FieldIsTooShort]),(BadPassword [FieldIsEmpty,FieldIsTooShort,FieldIsAllLower])])

  printValidation $ validateForm shortUsernameAndPassword
  -- > (Invalid [(BadUsername [FieldIsTooShort]),(BadPassword [FieldIsTooShort,FieldIsAllLower])])

  printValidation $ validateForm lowerCasePassword
  -- > (Invalid [(BadPassword [FieldIsAllLower])])

  printValidation $ validateForm upperCasePassword
  -- > (Invalid [(BadPassword [FieldIsAllUpper])])

  printValidation $ validateForm goodForm
  -- > (Valid "{\"username\":\"alice\",\"password\":\"FooBarBaz\"}")
  where
    printValidation = logShow <<< (map unsafeStringify)

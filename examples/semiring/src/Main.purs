module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Semiring.Free (Free, free)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String (length, null, toLower, toUpper)
import Data.Validation.Semiring (V, invalid)
import Global.Unsafe (unsafeStringify)
import Partial.Unsafe (unsafePartial)

-- | `UnvalidatedFormData` represents the raw data we might receive from a form
-- | before any validation has been performed.
-- |
-- | Note that both the `login` and `password` fields in this record are simple
-- | `String` types.
type UnvalidatedFormData =
  { loginIdent :: String
  , password   :: String
  }

-- | `LoginIdent` is a sum type representing the potential ways a user can
-- | identify themselves for login.
-- |
-- | For the sake of example here, a user can either identify themselves by
-- | their email address or username.
data LoginIdent
  = EmailAddress String
  | Username String

-- | `Password` is a wrapper around `String` that allows us to distinguish a
-- | field containing a valid password from any other potential `String`s.
newtype Password = Password String

-- | `ValidatedFormData` represents the valid data from a form that is produced
-- | as a result of our validation process.
-- |
-- | Note that the `username` and `password` fields that were simple `String`s
-- | in `UnvalidatedFormData` are now `Username` and `Password`, respectively.
type ValidatedFormData =
  { loginIdent :: LoginIdent
  , password   :: Password
  }

-- | `ValidationError` represents the potential errors we might encounter during
-- | the validation process.
data ValidationError
  = FieldIsEmpty
  | FieldIsTooShort
  | FieldIsAllLower
  | FieldIsAllUpper
  | FieldIsInvalidEmail

-- | Generically derive a `Show` instance for `ValidationError` so that we may
-- | print these errors to the console later.
derive instance genericValidationError :: Generic ValidationError _
instance showValidationError :: Show ValidationError where
  show = genericShow

-- | `ValidationErrors` is a helpful type alias for `Free` Semiring of the 
-- | errors we might encounter during the validation process.
type ValidationErrors = Free ValidationError

-- | A note on `Data.Validation.Semiring`'s `V`:
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
  | null input = invalid (free FieldIsEmpty)
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
  | length input <= validLength = invalid (free FieldIsTooShort)
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
  | toLower input == input = invalid (free FieldIsAllLower)
  | toUpper input == input = invalid (free FieldIsAllUpper)
  | otherwise = pure input

-- | This function validates that an input `String` is a valid email address
-- | by checking it against a regular expression.
-- |
-- | If the input isn't a valid email address, it returns a
-- | `FieldIsInvalidEmail` error on the `Invalid` side of `V`.
-- |
-- | Otherwise, it just returns the input on the `Valid` side of `V`.
validateEmailRegex :: String -> V ValidationErrors String
validateEmailRegex email
  | test emailRegex email = pure email
  | otherwise = invalid (free FieldIsInvalidEmail)

-- | A regular expression that validates email addresses.
emailRegex :: Regex
emailRegex = 
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"
  where 
  -- | Unsafely construct a regular expression from a pattern string.
  -- |
  -- | This will fail at runtime with an error if the pattern string is
  -- | invalid.
    unsafeRegexFromString :: String -> Regex
    unsafeRegexFromString str =
      let mkRegex = regex str noFlags
      in unsafePartial (fromRight mkRegex)

-- | `FormValidationError` represents the errors we might encounter while
-- | attempting to validate the username and password fields of our form.
-- |
-- | The `BadUsername` and `BadPassword` branches help us distinguish which
-- | part of the form failed validation.
data FormValidationError
  = BadEmailAddress ValidationErrors
  | BadUsername ValidationErrors
  | BadPassword ValidationErrors

-- | Generically derive a `Show` instance for `FormValidationError` so that we
-- | may print these errors to the console later.
derive instance genericFormValidationError :: Generic FormValidationError _
instance showFormValidationError :: Show FormValidationError where
  show = genericShow

-- | Much like `ValidationErrors`, `FormValidationErrors` is a helpful alias
-- | for a `Free` Semiring of errors specific to the validation of our form 
-- | fields.
type FormValidationErrors = Free FormValidationError

-- | This function validates that an input string conforms to our requirements
-- | for a valid email address. Namely, we require that the input be non-empty 
-- | and pass testing against the `emailRegex` defined above.
-- |
-- | If the input doesn't conform to these requirements, the failures
-- | encountered during validation will be collected on the `Invalid` side of
-- | `V`, tagged with a `BadEmailAddress` to identify the part of the form that
-- | failed validation, and wrapped in a `Free` Semiring so that additional 
-- | errors may be collected along with it.
-- |
-- | Otherwise, it returns the input wrapped in the `EmailAddress` constructor 
-- | for the `LoginIdent` data type to distinguish it from either a normal, 
-- | unvalidated `String` or a validated `Username`.
validateEmailAddress :: String -> V FormValidationErrors LoginIdent
validateEmailAddress input = bimap (free <<< BadEmailAddress) EmailAddress
  $  validateNonEmpty input
  *> validateEmailRegex input

-- | This function validates that an input string conforms to our requirements
-- | for a valid username. Namely, we require that the input be non-empty and at
-- | least 4 characters long.
-- |
-- | If the input doesn't conform to these requirements, the failures
-- | encountered during validation will be collected on the `Invalid` side of
-- | `V`, tagged with a `BadUsername` to identify the part of the form that
-- | failed validation, and wrapped in a `Free` Semiring so that additional 
-- | errors may be collected along with it.
-- |
-- | Otherwise, it returns the input wrapped in the `Username` constructor for
-- | the `LoginIdent` data type to distinguish it from either a normal, 
-- | unvalidated `String` or a validated `EmailAddress`.
validateUsername :: String -> V FormValidationErrors LoginIdent
validateUsername input = bimap (free <<< BadUsername) Username
  $  validateNonEmpty input
  *> validateMinimumLength 4 input

-- | This function validates that an input string conforms to our requirements
-- | for a valid login identifier. Namely, we require that the input pass
-- | either the `validateEmailAddress` or `validateUsername`
-- |
-- | Of note here is the fact that we use the `(<|>)` operator from `Control.Alt`
-- | to signify alternative validation functions for the same input.
-- |
-- | This is the crux of using the `Free` Semiring in the first place, as 
-- | Semiring provides a way for errors accumulated along either of these 
-- | alternative validation branches to be accumulated in the data structure
-- | and returned to the user in the event that it fails.
validateLoginIdent :: String -> V FormValidationErrors LoginIdent
validateLoginIdent input = 
  validateEmailAddress input <|> validateUsername input

-- | This function validates that an input string conforms to our requirements
-- | for a valid password. Namely, we require that the input be non-empty, at
-- | least 6 characters long, and contains both upper- and lower-case
-- | characters.
-- |
-- | If the input doesn't conform to these requirements, the failures
-- | encountered during validation will be collected on the `Invalid` side of
-- | `V`, tagged with a `BadPassword` to identify the part of the form that
-- | failed validation, and wrapped in a `Free` Semiring so that additional 
-- | errors may be collected along with it.
-- |
-- | Otherwise, it returns the input wrapped in the `Password` newtype to
-- | distinguish it from a normal, unvalidated `String`.
validatePassword :: String -> V FormValidationErrors Password
validatePassword input = bimap (free <<< BadPassword) Password
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
validateForm {loginIdent, password} = {loginIdent: _, password: _}
  <$> validateLoginIdent loginIdent
  <*> validatePassword password

-- | This is a form that will fail validation, since both fields are empty
-- | strings.
emptyUsernameAndPassword :: UnvalidatedFormData
emptyUsernameAndPassword = {loginIdent: "", password: ""}

-- | This is a form that will fail validation, since both fields are too short.
shortUsernameAndPassword :: UnvalidatedFormData
shortUsernameAndPassword = {loginIdent: "foo", password: "bar"}

-- | This is a form that will fail validation, since the password lowercase.
lowerCasePassword :: UnvalidatedFormData
lowerCasePassword = {loginIdent: "alice", password: "foobarbaz"}

-- | This is a form that will fail validation, since the password uppercase.
upperCasePassword :: UnvalidatedFormData
upperCasePassword = {loginIdent: "alice", password: "FOOBARBAZ"}

-- | This is a form with a username that will pass validation, as it conforms 
-- | to all the requirements outlined in the validation functions above.
goodUsernameForm :: UnvalidatedFormData
goodUsernameForm = {loginIdent: "alice", password: "FooBarBaz"}

-- | This is a form with an email address that will pass validation, as it 
-- | conforms to all the requirements outlined in the validation functions 
-- | above.
goodEmailAddressForm :: UnvalidatedFormData
goodEmailAddressForm = {loginIdent: "alice@example.com", password: "FooBarBaz"}

-- | Run through all of the example forms and print the validation results to
-- | the console.
-- |
-- | We'll cheat a little here and use `unsafeStringify` to get a `Show`able
-- | version of our `ValidatedFormData` record.
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  printValidation $ validateForm emptyUsernameAndPassword
  -- > Invalid ((Free (((BadEmailAddress (Free ((FieldIsEmpty : FieldIsInvalidEmail : Nil) : Nil))) : (BadPassword (Free ((FieldIsEmpty : FieldIsTooShort : FieldIsAllLower : Nil) : Nil))) : Nil) : ((BadUsername (Free ((FieldIsEmpty : FieldIsTooShort : Nil) : Nil))) : (BadPassword (Free ((FieldIsEmpty : FieldIsTooShort : FieldIsAllLower : Nil) : Nil))) : Nil) : Nil)))

  printValidation $ validateForm shortUsernameAndPassword
  -- > Invalid ((Free (((BadEmailAddress (Free ((FieldIsInvalidEmail : Nil) : Nil))) : (BadPassword (Free ((FieldIsTooShort : FieldIsAllLower : Nil) : Nil))) : Nil) : ((BadUsername (Free ((FieldIsTooShort : Nil) : Nil))) : (BadPassword (Free ((FieldIsTooShort : FieldIsAllLower : Nil) : Nil))) : Nil) : Nil)))

  printValidation $ validateForm lowerCasePassword
  -- > Invalid ((Free (((BadPassword (Free ((FieldIsAllLower : Nil) : Nil))) : Nil) : Nil)))

  printValidation $ validateForm upperCasePassword
  -- > Invalid ((Free (((BadPassword (Free ((FieldIsAllUpper : Nil) : Nil))) : Nil) : Nil)))

  printValidation $ validateForm goodUsernameForm
  -- > Valid ("{\"loginIdent\":{\"value0\":\"alice\"},\"password\":\"FooBarBaz\"}")

  printValidation $ validateForm goodEmailAddressForm
  -- > Valid ("{\"loginIdent\":{\"value0\":\"alice@example.com\"},\"password\":\"FooBarBaz\"}")
  where
    printValidation = logShow <<< (map unsafeStringify)

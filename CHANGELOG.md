# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#37 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:

## [v5.0.0](https://github.com/purescript/purescript-validation/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#28)

New features:

Bugfixes:

Other improvements:
- Added detailed `Semigroup` validation examples (#17)
- Added link to to Applicative Validation chapter in the PureScript book (#29) 
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#30)
- Renamed `unV` to `validation` and deprecated `unV` (#33)
- Added a changelog and pull request template (#34, #35)

## [v4.2.0](https://github.com/purescript/purescript-validation/releases/tag/v4.2.0) - 2019-01-24

- Exposed constructors for `V` types

## [v4.1.0](https://github.com/purescript/purescript-validation/releases/tag/v4.1.0) - 2019-01-14

- Add `andThen` for chaining validations (@colin-lamed)

## [v4.0.0](https://github.com/purescript/purescript-validation/releases/tag/v4.0.0) - 2018-05-24

- Updated for PureScript 0.12
- Added `toEither` functions
- Removed non-law-abiding `Alternative` instance for `Semiring` validation

## [v3.2.0](https://github.com/purescript/purescript-validation/releases/tag/v3.2.0) - 2017-11-06

- Added `Foldable` and `Traversable` instances

## [v3.1.0](https://github.com/purescript/purescript-validation/releases/tag/v3.1.0) - 2017-06-03

- Added `Monoid` instances (@safareli)

## [v3.0.0](https://github.com/purescript/purescript-validation/releases/tag/v3.0.0) - 2017-03-25

- Updated for PureScript 0.11

## [v2.0.0](https://github.com/purescript/purescript-validation/releases/tag/v2.0.0) - 2016-10-07

- Updated dependencies
- Added `Bifunctor` instances for the `V` types

## [v1.0.0](https://github.com/purescript/purescript-validation/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v0.2.1](https://github.com/purescript/purescript-validation/releases/tag/v0.2.1) - 2015-10-20

- Fixed warning (@anttih)

## [v0.2.0](https://github.com/purescript/purescript-validation/releases/tag/v0.2.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.1.1](https://github.com/purescript/purescript-validation/releases/tag/v0.1.1) - 2015-03-23

- Add `Data.Validation.Semiring` (@cryogenian)

## [v0.1.0](https://github.com/purescript/purescript-validation/releases/tag/v0.1.0) - 2015-03-20

- Updated docs

## [v0.0.3](https://github.com/purescript/purescript-validation/releases/tag/v0.0.3) - 2014-07-26

- Updated docs

## [v0.0.2](https://github.com/purescript/purescript-validation/releases/tag/v0.0.2) - 2014-04-26

- Removed test-related code

## [0.0.1](https://github.com/purescript/purescript-validation/releases/tag/0.0.1) - 2014-04-25

- Initial release

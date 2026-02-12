# Change Log

All notable changes to the "efmt" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [Unreleased]

## [0.21.0]

- Add support for EEP 79 (native records syntax)
- Add support for EEP 78 (multi-valued comprehensions)

## [0.19.1]

- Fix float tokenizing bug

## [0.19.0]

- Add support for EEP 75 (Based Floating Point Literals)
- Add support for EEP 73 (Zip generator)
- Add support for EEP-69 (Nominal Type)
- Add support for strict comprehensions generators
- Insert a blank line between `-include` and `-include_lib`

## [0.18.3]

- Remove extra newlines between `-doc` attributes and function definitions

## [0.18.2]

- Fix a bug when formatting consecutive `-define` directives with single-quoted macro names.

## [0.18.0]

- Add support for formatting escript files

## [0.17.0]

- Enable formatting of `?assertMatch()` and `?assertNotMatch()` macros even when they contain when in the pattern

## [0.15.0]

### Added

- Add support for triple-quoted strings (EEP-0064) that will be introduced in OTP-27
- Add support for sigil string literals (EEP-0066) that will be introduced in OTP-27

## [0.14.2]

### Fixed

- Specify activationEvents explicitly in package.json

## [0.14.1]

- Initial release

# Standard Library Test Support

Sources under this directory support the tests in `lib/tests/std/`. They either need supplied
arguments, filesystem fixtures, or input, or they are imported by a neighboring test. They do not
declare `@[package(test(...))]` and are not discovered as standalone tests.

Every `.zy` file under `lib/tests/std/` outside `support/` must be a test package.
`standard_library_test_sources_are_classified` in `cli/tests/package.rs` enforces that convention.

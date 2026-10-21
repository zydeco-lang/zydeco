# Standard Library Test Support

Sources under this directory support the tests in `lib/tests/std/`.
They either need supplied arguments, filesystem fixtures, or input, or they are imported by a neighboring test.
They do not declare `@[package(test(...))]`, so they are not standalone package tests.
Their leading directives declare the execution setup that `lang/tests/tests/std_directives.rs` supplies.

Every `.zy` file under `lib/tests/std/` outside `support/` must be a test package with `-- stage: run`.
`standard_library_test_sources_are_classified` in `cli/tests/package.rs` and `std/coverage`
in `lang/tests/tests/std_directives.rs` enforce that convention.

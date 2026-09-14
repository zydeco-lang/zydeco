# Formatter punning audit retirement

Review whether `NamedTermPunningAudit` still serves the formatter corpus tests after the library's punning migration.
The helper remains exported and used in [the formatter](../../lang/surface/src/textual/pretty.rs);
retiring the migration prose does not remove that code or establish that the test no longer needs it.

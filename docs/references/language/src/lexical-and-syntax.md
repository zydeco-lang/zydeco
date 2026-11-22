# Lexical Structure and Syntax

## Source Encoding and Comments

Source is UTF-8. Whitespace separates tokens, while indentation and line breaks do not determine syntax.
`--` starts a line comment; `/- ... -/` is a nestable block comment.
An uninterrupted `--|` block attaches text to a following `@[doc]`
or `@(literal)` ([Source Documentation](meta-annotations.md#source-documentation));
an intervening blank line or ordinary comment breaks attachment.

## Identifiers, Sigils, and Literals

Variable names start with an ASCII letter, or with `_` followed by at least one identifier character.
Subsequent characters are ASCII letters, digits, `_`, apostrophe, `?`, `+`, `*`, `-`, `=`, or `~`.
`_` alone is a hole.
A constructor starts with `+` and an uppercase letter; a destructor starts with `.` and a lowercase letter.
`#field` marks a field name wherever it could be confused with a binder.
Keywords are reserved.

Integer tokens are optionally signed decimal digits.
Floats require a fractional part with digits on both sides of the decimal point,
or a complete exponent: `1.25`, `1e3`, and `-2.5e-2` are valid.
Prefixes and suffixes such as `0x10` and `42u8` are rejected.
Strings use double quotes; characters use single quotes and contain one Unicode scalar value.
Both support `\\`, `\"`, `\'`, `\n`, `\r`, `\t`, `\0`, and `\u{hex}`, with one
to six hexadecimal digits denoting a Unicode scalar.

Meta annotations use `@[meta] e` on a payload term or `@(meta)` for a hole payload;
see [Meta Annotations](meta-annotations.md) for the complete catalog.

## Precedence and Grouping

The main precedence levels, from tightest to loosest, are:

| Forms | Grouping |
| --- | --- |
| Parentheses, braces, delimited terms | Explicit |
| `v/field` | Left |
| `! v`, `ret v`, `+C v` | Prefix |
| Application and `M .d` | Left |
| `A * B * C` | One flat product; explicit nesting is retained |
| `A -> B -> C` | Right |
| `v \|> f` | Left |
| `f <\| v` | Right; the right operand is a full term |
| `pi`, `val pi`, `forall`, `sigma`, `exists` | Binder bodies extend rightward |
| Abstractions, bindings, metadata | Extend over their following term |

Thus `!p/action x` means `(! (p/action)) x`.
The thin arrow `->` constructs a classifier; `=>` separates a function or matching header from its body.
Backward application, also called the backward pipe, accepts an abstraction, quantifier,
or binding directly after `<|`: `f <| fn x => body` groups as `f (fn x => body)`.
The abstraction's body extends rightward as usual.
Parenthesize the completed application when supplying another argument, as in `(f <| fn x => body) next`.

Annotations and named components have a separate grouping layer: `:` is non-associative
and binds more tightly than the right-associative `=` and `::` naming forms.
Expression annotations use parentheses, `(e : S)`, or the contents of a `begin ... end` block.
Write `((#x = 1) : (#x :: Int))` to annotate the whole named value; `(#x = 1 : Int)` annotates its payload.
Metadata in a juxtaposed application argument also needs parentheses; backward application admits it
as a full right operand.

## Field-Marker Rationale

The `#` marker keeps a field name from reading as a binder.
`=` already marks binding boundaries and both introduces and eliminates named components,
while field names draw on the same lexical space as variables.
Without a marker, a name on the left of `=` or `::` could be told apart as a field only by sort information.
Marking exactly those positions leaves announced positions bare: projections and puns carry `/`
or a leading `=`, so their field name is already identified.
The marker also completes the role-sigil family with `+Ctor` and `.dtor`.
Marking every field occurrence, including projection and pun positions, was rejected as redundant;
a distinct field connective would split the connective family; `.field` would collide with destructor lexing;
and restricting field names by case would break field puns, whose public name follows the payload's naming convention.

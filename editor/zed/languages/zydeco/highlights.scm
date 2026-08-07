; Comments and literals
(documentation_comment) @comment.doc
[
  (line_comment)
  (block_comment)
] @comment

[
  (integer_literal)
  (float_literal)
] @number
[
  (string_literal)
  (character_literal)
] @string

; Lexical identifier families. Cajun semantic tokens refine these roles when
; name resolution and type checking are available.
(upper_identifier) @type
(lower_identifier) @variable
(constructor_identifier) @constructor
(destructor_identifier) @function

[
  (hole)
  (hole_pattern)
] @variable.special

; Context visible from the concrete syntax tree.
(general_binding
  binder: (variable_pattern
    name: (_) @function)
  parameters: (copattern))

[
  (named_term
    name: (_) @property)
  (labeled_term
    name: (_) @property)
  (punned_term
    name: (_) @property)
  (named_pattern
    name: (_) @property)
  (punned_pattern
    name: (_) @property)
  (projection_expression
    field: (_) @property)
]

(meta_application
  callee: (meta_identifier) @function)
(metadata "@" @attribute)

; Keywords
[
  "end"
  "begin"
  "data"
  "codata"
  "as"
  "def"
  "define"
  "let"
  "param"
  "in"
  "that"
  "do"
  "do~"
  "ret"
  "monadic"
  "monadically"
  "fn"
  "pi"
  "fix"
  "match"
  "comatch"
  "forall"
  "exists"
  "sigma"
] @keyword

; Operators and punctuation
[
  "!"
  "*"
  "=>"
  "->"
  "."
  "/"
  "::"
  "<-"
  "="
] @operator

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ":"
  ";"
] @punctuation.delimiter

"|" @punctuation.list_marker

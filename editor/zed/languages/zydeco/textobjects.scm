(
  (context_binding
    kind: (binding_kind) @_kind
    binding: (general_binding
      value: (_) @function.inside)) @function.around
  (#match? @_kind "^(def|define)$")
)

(
  (context_binding
    kind: (binding_kind) @_kind
    binding: (general_binding
      value_function: (value_modifier)
      value: (_) @function.inside)) @function.around
  (#eq? @_kind "let")
)

(value_lambda_expression
  body: (_) @function.inside) @function.around

[
  (data_type)
  (codata_type)
] @class.around

(documentation_comment)+ @comment.around
(line_comment)+ @comment.around
(block_comment) @comment.around

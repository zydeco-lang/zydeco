(
  (context_binding
    kind: (binding_kind) @context @_kind
    binding: (general_binding
      binder: (variable_pattern
        name: (_) @name))) @item
  (#match? @_kind "^(def|define)$")
)

(
  (context_binding
    kind: (binding_kind) @context @_kind
    binding: (general_binding
      value_function: (value_modifier)
      binder: (variable_pattern
        name: (_) @name))) @item
  (#eq? @_kind "let")
)

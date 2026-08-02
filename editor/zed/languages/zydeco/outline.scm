(
  (context_binding
    kind: (binding_kind) @context @_kind
    binding: (general_binding
      binder: (variable_pattern
        name: (_) @name))) @item
  (#match? @_kind "^(def|define)$")
)

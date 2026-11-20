# Translations into the Core

Two meta annotations give their payload a reading other than the core's own and translate it
into the core; a third is reserved for another.
`@[monadic] e` reinterprets the returners in `e` through a relative monad that the block takes as a parameter.
`@[cbv] e` reads `e` as a call-by-value expression and elaborates it, writing its evaluation order into the result.
`@[cbn] e` is reserved for the call-by-name counterpart and is refused until that exists.
Each translation runs in one direction and produces ordinary core syntax, which is checked,
linked and run like any other; that is also why they compose.
A call-by-value block nested in a monadic block is translated by both, and that composition is
where a call-by-value block's evaluation order meets effects.

# Thoughts on Modules

This blog contains random ideas on designing modules in Zydeco, spanning the purpose, functionality, semantics, and implications.

## Motivation

The modules are compilation units that provide boundaries for the compilation and linkage process. They serve as elementary components for metaprogramming. They also act as tools for organizing code pieces in growing projects, presented as a grouping structure and controlling encapsulation and visibility. Vital as it sounds, it's more difficult than not to "do modules the correct way," and that's why we hear all the debates over this topic. What people are arguing can generally be categorized into the following:

1. Leakage of abstractions. If a type defined inside a module is not publicly available, then how should we accidentally be able to use it outside of the module?
2. Time of the import, especially regarding side effects. E.g. when can I expect the side effects of my lazy-loaded module's initiation to occur?
3. Cyclic dependency. How can we convince the compiler that it makes sense as an abstraction without manually creating a Landin's knot?
4. Form of representation. Should a file serve as a module, or should we treat the repo as a flattened ocean of declarations?

The design of modules in Zydeco should keep these questions in mind and try its best to illuminate all the dark corners of language designing - though it's too hard to achieve, and more often than not, it's just a bunch of tradeoffs.

## Our Attempts

### Iteration No. 1:  Definitions and Entry Points

Before defining how modules work, we should first review (or reimagine?) the big picture. The package is a common abstraction for modern software, mainly used for distribution purposes. Let's borrow this concept for a while.

Suppose that a package umbrellas all Zydeco code pieces in a project. Inside which, all values and computations should be bound under some definition except one computation, which would be the entry point. We can make the entry point a new form of definition that can only be present once, and its type depends on the context. For example,

```
def sum = { fn (xs : List Int) ->
    match xs
    | Nil() -> 0
    | Cons(x, xs) -> (
        do y <- !sum xs;
        !add x y
    )
} end

entry <-
    do x <- ! sum [1, 2, 3];
    do s <- ! int_to_str x;
    ! writeline s { ! exit 0 }
end
```

Languages such as Rust distinguishes between binaries and a library at the package level because it involves how to compile and link them since binaries can run and libraries can't - or can't they?

In fact, many libraries need to perform side effects to actually be meaningful, and most either rely on the import mechanism or require the programmer to bind or call the hook functions. So if the need is natural, why don't we generalize the form of our program and make the concept of entry point baked-in?

At the moment, we don't know what the modules look like, but **they should be composed of a collection of definitions, where there is optionally one entry point**. We may also need a new form of computation to call a module's entry point.

There is another implication of this design. If no entry point is found in a module, it's a pure library. One of the cool features that CBPV languages possess is the ability to control when side effects can happen, which significantly benefits the design of our modules since definitions are values that don't step.

### Iteration No. 2: Mutual Recursion

As discussed before, we should provide mutual recursion on definitions, thus at the module level. The implementation would involve generating a codata type and a corresponding codata term (maybe we can call it stem?) and optionally inlining the call to the stem upon each usage.

### Iteration No. 3: Namespaces and Visibility

...

Now that modules are more like definitions, we can also propose them to be able to cascade, which can be flattened to top level definitions.

### Iteration No. 4: Module Signatures and First Class Modules

Hardly ML level modules, but let's try...

## Comments

(: Write any comment below :)
(: Why isn't there a language that uses smiling faces as the comment syntax... What a pity... :)
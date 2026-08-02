Since continuations are mentioned in the README, I want to put some notes down about how to think of them in the context of CBPV.

In CBV languages, delimited continuations are a built-in feature of the compiler. We can think of them as being part of the "ambient" monad that the CBV programming language is always working with.

In CBPV we have 2 choices:
1. We can support delimited continuations as part of the `F` types.
2. We can *implement* delimited continuations *inside* CBPV as a type `DelimCont : ValTy -> CompTy`. See the abstract and slides of my recent talk at HOPE for some ideas along these lines (http://maxsnew.com/publications.html#hope22) (not explicitly about delimited continuations)

So for the time being, I would say it is more in line with our research goals to see if we can make (2) fast then to implement (1) in the compiler. A good starting point for what the API should be is Oleg's paper implementing multi-prompt delimited continuations in OCaml: https://okmij.org/ftp/continuations/implementations.html#delimcc-paper 

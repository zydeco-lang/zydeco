$
  #text("Value")       & V & ::=
    & x
    | { M }
    | ...
    \
  #text("Stack")       & S & ::=
    & "curr"
    | lambda x . M
    | S; "push" V
    | ...
    \
  #text("Computation") & M & ::=
    & V space ! space S
    | S space @ space V
    | x <- "pop" S; M
    | ...
    \
$
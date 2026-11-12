In R a comparison with NaN is NA, not false, and `if (NA)` stops the program; the R port runs a fast engine and reruns a safe one when that happens.

C defines every ordered comparison with a NaN as false and `!=` as true, and EPA SWMM 5.2.4 relies on it: a NaN
depth or flow passes through `if (x < y)` tests and comes out in the report as `nan` or as a value chosen by the
false branch. In R, `NaN < 1` is `NA`; `if (NA)` stops with "missing value where TRUE/FALSE needed"; `NA && TRUE`
is `NA` while `NA && FALSE` is `FALSE`, and `!NA` is `NA`. A literal translation therefore either agrees with C or
stops: it never gives a wrong answer silently, but it stops on any model that makes a NaN.

Writing every comparison the C way costs a test of `is.na` on each one. The emitter (`tools/c2x/emit_r.py`) writes
both: `r/swmm5/engine.R`, the safe engine, spells `a < b` as

    (!is.na(.c <- (a < b)) && .c)

and `a != b` as `(is.na(.c <- (a != b)) || .c)`, and its double-to-int conversion tests `is.na` too;
`r/swmm5/engine_fast.R` is the same program with plain R comparisons. `r/runswmm.R` runs the fast engine, and if it
stops with an error it closes the files, removes the temporary files, and runs the model again from the start on the
safe engine, whose outputs replace the first run's. EPA's model Hydraulics/swmm5_shapes.inp does this:

    $ SWMM_R_VERBOSE=1 r/runswmm swmm5_shapes.inp r.rpt r.out
    fast engine stopped (missing value where TRUE/FALSE needed); running the safe engine

and its .rpt and .out are then identical to EPA's.

The rerun is only correct if the fast engine cannot be wrong without stopping. Where the NA's final value is
decided, it is: `if`, `while` and the conversion of a truth value to a number all stop on NA, and `NA && FALSE`
(FALSE) and `NA || TRUE` (TRUE) are what C gives whichever truth value the NaN had in C. The hole is evaluation
order: R evaluates the right operand of `NA && x` and of `NA || x`, while C evaluates it only after a true left
operand of `&&` or a false one of `||`. If the right operand has a side effect (a call, an assignment), the fast
engine can run it when C would not, and then answer FALSE or TRUE without stopping. A plain comparison with NaN is
false in C, so `||` is safe and `&&` is not; under a `!=`, a `!`, or a floating value tested as a condition, NaN is
true in C and `||` is the unsafe one. The emitter writes such a left operand with the safe spelling in the fast
engine too. The first version did this for `&&` only; the `||` case turned up while this lesson was written, and
regenerating with the wider rule (`has_negation` in `emit_r.py`) gave byte-identical engines, so EPA's code has no
such `||` and the gate runs stand.

What to take from it: when a target language's NaN rule differs from C's, find out whether the difference is loud
or silent. A loud one can be handled by running the cheap translation and falling back to the exact one; check
evaluation order too, because a short-circuit operator can do the wrong thing before anything is loud.

GHC's inverse hyperbolic trigonometric functions
================================================

Some of the mathematical functions defined in GHC's library exhibit
poor performance.

Original definitions
--------------------

`asinh` is defined in [GHC.Float](https://hackage.haskell.org/package/base/docs/src/GHC.Float.html#line-478) as

    asinh x = log (x + sqrt (1.0+x*x))

which, unsurprisingly, has problems for large negative values of `x`.
Similarly, `atanh` is defined as

    atanh x = 0.5 * log ((1.0+x) / (1.0-x))

which exhibits similar problems.

Proposed improvements
---------------------

This package contains versions of these functions which are much more
accurate, implemented via the same logic as GNU libm.  See
[Hyperbolic.hs](https://github.com/peddie/ghc-inverse-hyperbolic/blob/master/src/Hyperbolic.hs]
for their definitions.

### `log1p`

An accurate version of the standard `log1p` function was needed to
implement `asinh` and `atanh`.

Error measurements
------------------

Each implementation (GHC, GNU libm, candidate replacement) was
compared at a handful of grid points to a "truth" value as calculated
by GNU MPFR to high precision.  This package contains an executable
called `generateInverseHyperbolicErrorPlots` (yikes) which generates,
for each function, a plot of log error magnitude vs. log of the
argument along with a plot of the log of error in ULP (units in the
last place, or bits of precision) vs. log of the argument.

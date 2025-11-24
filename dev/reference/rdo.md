# Execute expressions of random variables

Execute (nearly) arbitrary R expressions that may include
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s,
producing a new
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Usage

``` r
rdo(expr, dim = NULL, ndraws = NULL)
```

## Arguments

- expr:

  (expression) A bare expression that can (optionally) contain
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s. The
  expression supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html).

- dim:

  (integer vector) One or more integers giving the maximal indices in
  each dimension to override the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to be
  created (see [`dim()`](https://rdrr.io/r/base/dim.html)). If `NULL`
  (the default), `dim` is determined by the input. **NOTE:** This
  argument controls the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), not the
  underlying array, so you cannot change the number of draws using this
  argument.

- ndraws:

  (positive integer) The number of draws used to construct new random
  variables if no
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s are
  supplied in `expr`. If `NULL`, `getOption("posterior.rvar_ndraws")` is
  used (default 4000). If `expr` contains
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, the
  number of draws in the provided
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s is used
  instead of the value of this argument.

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Details

This function evaluates `expr` possibly multiple times, once for each
draw of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s it
contains, then returns a new
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
representing the output of those expressions. To identify
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, `rdo()`
searches the calling environment for any variables named in `expr` for
which
[`is_rvar()`](https://mc-stan.org/posterior/dev/reference/is_rvar.md)
evaluates to `TRUE`. If `expr` contains no
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, then it
will be executed `ndraws` times and an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with that
many draws returned.

`rdo()` is not necessarily *fast* (in fact in some cases it may be very
slow), but it has the advantage of allowing a nearly arbitrary R
expression to be executed against
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s simply by
wrapping it with `rdo( ... )`. This makes it especially useful as a
prototyping tool. If you create code with `rdo()` and it is unacceptably
slow for your application, consider rewriting it using math operations
directly on
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s (which
should be fast), using
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md),
and/or using operations directly on the arrays that back the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s (via
[`draws_of()`](https://mc-stan.org/posterior/dev/reference/draws_of.md)).

## See also

Other rfun:
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md),
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)

## Examples

``` r
mu <- rdo(rnorm(10, mean = 1:10, sd = 1))
sigma <- rdo(rgamma(1, shape = 1, rate = 1))
x <- rdo(rnorm(10, mu, sigma))
x
#> rvar<4000>[10] mean ± sd:
#>  [1]  1 ± 1.7   2 ± 1.7   3 ± 1.7   4 ± 1.7   5 ± 1.7   6 ± 1.7   7 ± 1.7 
#>  [8]  8 ± 1.7   9 ± 1.7  10 ± 1.7 
```

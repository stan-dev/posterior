# Create random variables from existing random number generators

Specialized alternative to
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) or
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md) for
creating [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s
from existing random-number generator functions (such as
[`rnorm()`](https://rdrr.io/r/stats/Normal.html),
[`rbinom()`](https://rdrr.io/r/stats/Binomial.html), etc).

## Usage

``` r
rvar_rng(.f, n, ..., ndraws = NULL)
```

## Arguments

- .f:

  (function) A function (or string naming a function) representing a
  random-number generating function that follows the pattern of base
  random number generators (like
  [`rnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`rbinom()`](https://rdrr.io/r/stats/Binomial.html), etc). It must:

  - Have a first argument, `n`, giving the number of draws to take from
    the distribution

  - Have vectorized parameter arguments

  - Return a single vector of length `n`

- n:

  (positive integer) The length of the output
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) vector
  (**not** the number of draws).

- ...:

  Arguments passed to `.f`. These arguments may include
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, so
  long as they are vectors only (no multidimensional
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s are
  allowed).

- ndraws:

  (positive integer) The number of draws used to construct the returned
  random variable if no
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s are
  supplied in `...`. If `NULL`, `getOption("posterior.rvar_ndraws")` is
  used (default 4000). If `...` contains
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, the
  number of draws in the provided
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s is used
  instead of the value of this argument.

## Value

A single-dimensional
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) of length
`n`.

## Details

This function unwraps the arrays underlying the input
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s in `...`
and then passes them to `.f`, relying on the vectorization of `.f` to
evaluate it across draws from the input
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s. This is
why the arguments of `.f` **must** be vectorized. It asks for `n` times
the number of draws in the input
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s (or
`ndraws` if none are given) draws from the random number generator `.f`,
then reshapes the output from `.f` into an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with
length `n`.

`rvar_rng()` is a fast alternative to
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) or
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), but you
**must** ensure that `.f` satisfies the preconditions described above
for the result to be correct. Most base random number generators satisfy
these conditions. It is advisable to test against
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) or
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md) (which
should be correct, but slower) if you are uncertain.

## See also

Other rfun:
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md)

## Examples

``` r
mu <- rvar_rng(rnorm, 10, mean = 1:10, sd = 1)
sigma <- rvar_rng(rgamma, 1, shape = 1, rate = 1)
x <- rvar_rng(rnorm, 10, mu, sigma)
x
#> rvar<4000>[10] mean ± sd:
#>  [1]  0.94 ± 1.8   2.02 ± 1.8   2.98 ± 1.7   4.02 ± 1.7   5.01 ± 1.7 
#>  [6]  6.00 ± 1.8   7.04 ± 1.7   7.98 ± 1.7   9.00 ± 1.8   9.98 ± 1.7 
```

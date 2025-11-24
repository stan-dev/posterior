# Create functions of random variables

Function that create functions that can accept and/or produce
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

## Usage

``` r
rfun(.f, rvar_args = NULL, rvar_dots = TRUE, ndraws = NULL)
```

## Arguments

- .f:

  (multiple options) A function to turn into a function that accepts
  and/or produces random variables:

  - A function

  - A one-sided formula that can be parsed by
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html)

- rvar_args:

  (character vector) The names of the arguments of `.f` that should be
  allowed to accept
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s as
  arguments. If `NULL` (the default), all arguments to `.f` are turned
  into arguments that accept
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s,
  including arguments passed via `...` (if `rvar_dots` is `TRUE`).

- rvar_dots:

  (logical) Should dots (`...`) arguments also be converted? Only
  applies if `rvar_args` is `NULL` (i.e., all arguments are allowed to
  be [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s).

- ndraws:

  (positive integer). The number of draws used to construct new random
  variables if no
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s are
  supplied as arguments to the returned function. If `NULL`,
  `getOption("posterior.rvar_ndraws")` is used (default `4000`). If any
  arguments to the returned function contain
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, the
  number of draws in the provided
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s is used
  instead of the value of this argument.

## Value

A function with the same argument specification as `.f`, but which can
accept and return
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

## Details

This function wraps an existing function (`.f`) such that it returns
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s
containing whatever type of data `.f` would normally return.

The returned function, when called, executes `.f` possibly multiple
times, once for each draw of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s passed to
it, then returns a new
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
representing the output of those function evaluations. If the arguments
contain no
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, then
`.f` will be executed `ndraws` times and an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with that
many draws returned.

Functions created by `rfun()` are not necessarily *fast* (in fact in
some cases they may be very slow), but they have the advantage of
allowing a nearly arbitrary R functions to be executed against
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s simply by
wrapping them with `rfun()`. This makes it especially useful as a
prototyping tool. If you create code with `rfun()` and it is
unacceptably slow for your application, consider rewriting it using math
operations directly on
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s (which
should be fast), using
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md),
and/or using operations directly on the arrays that back the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s (via
[`draws_of()`](https://mc-stan.org/posterior/dev/reference/draws_of.md)).

## See also

Other rfun:
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)

## Examples

``` r
rvar_norm <- rfun(rnorm)
rvar_gamma <- rfun(rgamma)

mu <- rvar_norm(10, mean = 1:10, sd = 1)
sigma <- rvar_gamma(1, shape = 1, rate = 1)
x <- rvar_norm(10, mu, sigma)
x
#> rvar<4000>[10] mean ± sd:
#>  [1]  0.97 ± 1.8   2.03 ± 1.8   2.99 ± 1.7   3.99 ± 1.7   4.97 ± 1.7 
#>  [6]  6.03 ± 1.8   6.99 ± 1.8   8.01 ± 1.8   8.98 ± 1.7   9.98 ± 1.7 
```

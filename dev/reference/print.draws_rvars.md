# Print `draws_rvars` objects

Pretty printing for
[`draws_rvars`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
objects.

## Usage

``` r
# S3 method for class 'draws_rvars'
print(
  x,
  digits = 2,
  max_variables = getOption("posterior.max_variables", 8),
  summary = getOption("posterior.rvar_summary", "mean_sd"),
  reserved = FALSE,
  ...
)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- digits:

  (nonnegative integer) The minimum number of significant digits to
  print. If `NULL`, defaults to `getOption("posterior.digits", 2)`.

- max_variables:

  (positive integer) The maximum number of variables to print. Can be
  controlled globally via the `"posterior.max_variables"`
  [option](https://rdrr.io/r/base/options.html).

- summary:

  (string) The style of summary to display:

  - `"mean_sd"` displays `mean ± sd`

  - `"median_mad"` displays `median ± mad`

  - `"mode_entropy"` displays `mode <entropy>`, and is used
    automatically for
    [`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)s.
    It shows normalized entropy, which ranges from 0 (all probability in
    one category) to 1 (uniform). See
    [`entropy()`](https://mc-stan.org/posterior/dev/reference/entropy.md).

  - `"mode_dissent"` displays `mode <dissent>`, and is used
    automatically for
    [`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)s.
    It shows Tastle and Wierman's (2007) *dissention* measure, which
    ranges from 0 (all probability in one category) through 0.5
    (uniform) to 1 (bimodal: all probability split equally between the
    first and last category). See
    [`dissent()`](https://mc-stan.org/posterior/dev/reference/dissent.md).

  - `NULL` uses `getOption("posterior.rvar_summary")` (default
    `"mean_sd`)

- reserved:

  (logical) Should reserved variables be included in the output?
  Defaults to `FALSE`. See
  [`reserved_variables`](https://mc-stan.org/posterior/dev/reference/reserved_variables.md)
  for an overview of currently reserved variable names.

- ...:

  Further arguments passed to the underlying
  [`print()`](https://rdrr.io/r/base/print.html) methods.

## Value

A `draws` object of the same class as `x`.

## Examples

``` r
x <- as_draws_rvars(example_draws())
print(x)
#> # A draws_rvars: 100 iterations, 4 chains, and 3 variables
#> $mu: rvar<100,4>[1] mean ± sd:
#> [1] 4.2 ± 3.4 
#> 
#> $tau: rvar<100,4>[1] mean ± sd:
#> [1] 4.2 ± 3.6 
#> 
#> $theta: rvar<100,4>[8] mean ± sd:
#> [1] 6.7 ± 6.3  5.3 ± 4.6  3.0 ± 6.8  4.9 ± 4.9  3.2 ± 5.1  4.0 ± 5.2  6.5 ± 5.3 
#> [8] 4.6 ± 5.3 
#> 
```

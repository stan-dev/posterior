# Pareto-smoothed probability integral transform

Compute PIT values using the empirical CDF, then refine values in the
tails by fitting a generalized Pareto distribution (GPD) to the tail
draws. This gives smoother, more accurate PIT values in the tails where
the ECDF is coarse, and avoids PIT values of 0 and 1. Due to use of
generalized Pareto distribution CDF in tails, the PIT values are not
anymore rank based and continuous uniformity test is appropriate.

## Usage

``` r
pareto_pit(x, y, ...)

# Default S3 method
pareto_pit(x, y, weights = NULL, log = FALSE, ndraws_tail = NULL, ...)

# S3 method for class 'draws_matrix'
pareto_pit(x, y, weights = NULL, log = FALSE, ndraws_tail = NULL, ...)

# S3 method for class 'rvar'
pareto_pit(x, y, weights = NULL, log = FALSE, ndraws_tail = NULL, ...)
```

## Arguments

- x:

  (draws) A
  [`draws_matrix`](https://mc-stan.org/posterior/reference/draws_matrix.md)
  object or one coercible to a `draws_matrix` object, or an
  [`rvar`](https://mc-stan.org/posterior/reference/rvar.md) object.

- y:

  (observations) A 1D vector, or an array of dim(x), if x is `rvar`.
  Each element of `y` corresponds to a variable in `x`.

- ...:

  Arguments passed to individual methods (if applicable).

- weights:

  A matrix of weights for each draw and variable. `weights` should have
  one column per variable in `x`, and `ndraws(x)` rows.

- log:

  (logical) Are the weights passed already on the log scale? The default
  is `FALSE`, that is, expecting `weights` to be on the standard
  (non-log) scale.

- ndraws_tail:

  (integer) Number of tail draws to use for GPD fitting. If `NULL` (the
  default), computed using
  [`ps_tail_length()`](https://mc-stan.org/posterior/reference/ps_tail_length.md).

## Value

A numeric vector of length `length(y)` containing the PIT values, or an
array of shape `dim(y)`, if `x` is an `rvar`.

## Details

The function first computes raw PIT values identically to
[`pit()`](https://mc-stan.org/posterior/reference/pit.md) (including
support for weighted draws). It then fits a GPD to both tails of the
draws (using the same approach as
[`pareto_smooth()`](https://mc-stan.org/posterior/reference/pareto_smooth.md))
and replaces PIT values for observations falling in the tail regions:

For a right-tail observation \\y_i \> c_R\\ (where \\c_R\\ is the
right-tail cutoff):

\$\$PIT(y_i) = 1 - p\_{tail}(1 - F\_{GPD}(y_i; c_R, \sigma_R, k_R))\$\$

For a left-tail observation \\y_i \< c_L\\:

\$\$PIT(y_i) = p\_{tail}(1 - F\_{GPD}(-y_i; -c_L, \sigma_L, k_L))\$\$

where \\p\_{tail}\\ is the proportion of (weighted) mass in the tail.

When (log-)weights in `weights` are provided, they are used for the raw
PIT computation (as in
[`pit()`](https://mc-stan.org/posterior/reference/pit.md)) and for GPD
fit.

## See also

[`pit()`](https://mc-stan.org/posterior/reference/pit.md) for the
unsmoothed version,
[`pareto_smooth()`](https://mc-stan.org/posterior/reference/pareto_smooth.md)
for Pareto smoothing of draws.

## Examples

``` r
x <- example_draws()
y <- rnorm(nvariables(x), 5, 5)
pareto_pit(x, y)
#>        mu       tau  theta[1]  theta[2]  theta[3]  theta[4]  theta[5]  theta[6] 
#> 0.1117828 0.1575000 0.3200000 0.9680722 0.8300000 0.8707061 0.5875000 0.1074550 
#>  theta[7]  theta[8] 
#> 0.7025000 0.4475000 
```

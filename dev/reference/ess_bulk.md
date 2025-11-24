# Bulk effective sample size (bulk-ESS)

Compute a bulk effective sample size estimate (bulk-ESS) for a single
variable. Bulk-ESS is useful as a diagnostic for the sampling efficiency
in the bulk of the posterior. It is defined as the effective sample size
for rank normalized values using split chains. For the tail effective
sample size see
[`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md).
See Vehtari (2021) for an in-depth comparison of different effective
sample size estimators.

## Usage

``` r
ess_bulk(x, ...)

# Default S3 method
ess_bulk(x, ...)

# S3 method for class 'rvar'
ess_bulk(x, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

If the input is an array, returns a single numeric value. If any of the
draws is non-finite, that is, `NA`, `NaN`, `Inf`, or `-Inf`, the
returned output will be (numeric) `NA`. Also, if all draws within any of
the chains of a variable are the same (constant), the returned output
will be (numeric) `NA` as well. The reason for the latter is that, for
constant draws, we cannot distinguish between variables that are
supposed to be constant (e.g., a diagonal element of a correlation
matrix is always 1) or variables that just happened to be constant
because of a failure of convergence or other problems in the sampling
process.

If the input is an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), returns
an array of the same dimensions as the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), where
each element is equal to the value that would be returned by passing the
draws array for that element of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to this
function.

## References

Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
Paul-Christian Bürkner (2021). Rank-normalization, folding, and
localization: An improved R-hat for assessing convergence of MCMC (with
discussion). *Bayesian Analysis*. 16(2), 667-–718. doi:10.1214/20-BA1221

Aki Vehtari (2021). Comparison of MCMC effective sample size estimators.
Retrieved from https://avehtari.github.io/rhat_ess/ess_comparison.html

## See also

Other diagnostics:
[`ess_basic()`](https://mc-stan.org/posterior/dev/reference/ess_basic.md),
[`ess_quantile()`](https://mc-stan.org/posterior/dev/reference/ess_quantile.md),
[`ess_sd()`](https://mc-stan.org/posterior/dev/reference/ess_sd.md),
[`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md),
[`mcse_mean()`](https://mc-stan.org/posterior/dev/reference/mcse_mean.md),
[`mcse_quantile()`](https://mc-stan.org/posterior/dev/reference/mcse_quantile.md),
[`mcse_sd()`](https://mc-stan.org/posterior/dev/reference/mcse_sd.md),
[`pareto_diags()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
[`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md),
[`rhat_basic()`](https://mc-stan.org/posterior/dev/reference/rhat_basic.md),
[`rhat_nested()`](https://mc-stan.org/posterior/dev/reference/rhat_nested.md),
[`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md)

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
ess_bulk(mu)
#> [1] 558.0173

d <- as_draws_rvars(example_draws("multi_normal"))
ess_bulk(d$Sigma)
#>          [,1]     [,2]     [,3]
#> [1,] 742.2907 454.0657 468.3890
#> [2,] 454.0657 528.7972 434.1141
#> [3,] 468.3890 434.1141 728.9440
```

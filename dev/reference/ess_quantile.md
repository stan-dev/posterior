# Effective sample sizes for quantiles

Compute effective sample size estimates for quantile estimates of a
single variable.

## Usage

``` r
ess_quantile(x, probs = c(0.05, 0.95), ...)

# Default S3 method
ess_quantile(x, probs = c(0.05, 0.95), names = TRUE, ...)

# S3 method for class 'rvar'
ess_quantile(x, probs = c(0.05, 0.95), names = TRUE, ...)

ess_median(x, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- probs:

  (numeric vector) Probabilities in `[0, 1]`.

- ...:

  Arguments passed to individual methods (if applicable).

- names:

  (logical) Should the result have a `names` attribute? The default is
  `TRUE`, but use `FALSE` for improved speed if there are many values in
  `probs`.

## Value

If the input is an array, returns a numeric vector with one element per
quantile. If any of the draws is non-finite, that is, `NA`, `NaN`,
`Inf`, or `-Inf`, the returned output will be a vector of (numeric) `NA`
values. Also, if all draws of a variable are the same (constant), the
returned output will be a vector of (numeric) `NA` values as well. The
reason for the latter is that, for constant draws, we cannot distinguish
between variables that are supposed to be constant (e.g., a diagonal
element of a correlation matrix is always 1) or variables that just
happened to be constant because of a failure of convergence or other
problems in the sampling process.

If the input is an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) and
`length(probs) == 1`, returns an array of the same dimensions as the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), where
each element is equal to the value that would be returned by passing the
draws array for that element of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to this
function. If `length(probs) > 1`, the first dimension of the result
indexes the input probabilities; i.e. the result has dimension
`c(length(probs), dim(x))`.

## References

Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
Paul-Christian Bürkner (2021). Rank-normalization, folding, and
localization: An improved R-hat for assessing convergence of MCMC (with
discussion). *Bayesian Analysis*. 16(2), 667-–718. doi:10.1214/20-BA1221

## See also

Other diagnostics:
[`ess_basic()`](https://mc-stan.org/posterior/dev/reference/ess_basic.md),
[`ess_bulk()`](https://mc-stan.org/posterior/dev/reference/ess_bulk.md),
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
ess_quantile(mu, probs = c(0.1, 0.9))
#>  ess_q10  ess_q90 
#> 300.6674 325.0324 

d <- as_draws_rvars(example_draws("multi_normal"))
ess_quantile(d$mu, probs = c(0.1, 0.9))
#>             [,1]     [,2]     [,3]
#> ess_q10 383.4835 468.2163 340.6056
#> ess_q90 389.0418 419.6722 271.5482
```

# Monte Carlo standard error for quantiles

Compute Monte Carlo standard errors for quantile estimates of a single
variable.

## Usage

``` r
mcse_quantile(x, probs = c(0.05, 0.95), ...)

# Default S3 method
mcse_quantile(x, probs = c(0.05, 0.95), names = TRUE, ...)

# S3 method for class 'rvar'
mcse_quantile(x, probs = c(0.05, 0.95), names = TRUE, ...)

mcse_median(x, ...)
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
[`ess_quantile()`](https://mc-stan.org/posterior/dev/reference/ess_quantile.md),
[`ess_sd()`](https://mc-stan.org/posterior/dev/reference/ess_sd.md),
[`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md),
[`mcse_mean()`](https://mc-stan.org/posterior/dev/reference/mcse_mean.md),
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
mcse_quantile(mu, probs = c(0.1, 0.9))
#>  mcse_q10  mcse_q90 
#> 0.3199011 0.2677437 

d <- as_draws_rvars(example_draws("multi_normal"))
mcse_quantile(d$mu)
#>                 [,1]       [,2]       [,3]
#> mcse_q5  0.012675014 0.01279547 0.02331627
#> mcse_q95 0.008302994 0.02609606 0.05555146
```

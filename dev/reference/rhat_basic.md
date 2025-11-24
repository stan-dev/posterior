# Basic version of the Rhat convergence diagnostic

Compute the basic Rhat convergence diagnostic for a single variable as
described in Gelman et al. (2013) with some changes according to Vehtari
et al. (2021). For practical applications, we strongly recommend the
improved Rhat convergence diagnostic implemented in
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md).

## Usage

``` r
rhat_basic(x, ...)

# Default S3 method
rhat_basic(x, split = TRUE, ...)

# S3 method for class 'rvar'
rhat_basic(x, split = TRUE, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Arguments passed to individual methods (if applicable).

- split:

  (logical) Should the estimate be computed on split chains? The default
  is `TRUE`.

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

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). *Bayesian Data Analysis, Third
Edition*. Chapman and Hall/CRC.

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
[`mcse_quantile()`](https://mc-stan.org/posterior/dev/reference/mcse_quantile.md),
[`mcse_sd()`](https://mc-stan.org/posterior/dev/reference/mcse_sd.md),
[`pareto_diags()`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md),
[`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md),
[`rhat_nested()`](https://mc-stan.org/posterior/dev/reference/rhat_nested.md),
[`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md)

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
rhat_basic(mu)
#> [1] 0.9979106

d <- as_draws_rvars(example_draws("multi_normal"))
rhat_basic(d$Sigma)
#>           [,1]      [,2]      [,3]
#> [1,] 0.9967529 0.9967817 1.0033460
#> [2,] 0.9967817 0.9950557 0.9998587
#> [3,] 1.0033460 0.9998587 0.9947308
```

# Nested Rhat convergence diagnostic

Compute the nested Rhat convergence diagnostic for a single variable as
proposed in Margossian et al. (2024).

## Usage

``` r
rhat_nested(x, ...)

# Default S3 method
rhat_nested(x, superchain_ids, ...)

# S3 method for class 'rvar'
rhat_nested(x, superchain_ids, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Arguments passed to individual methods (if applicable).

- superchain_ids:

  (numeric) Vector of length nchains specifying which superchain each
  chain belongs to. There should be equal numbers of chains in each
  superchain. All chains within the same superchain are assumed to have
  been initialized at the same point.

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

## Details

Nested Rhat is a convergence diagnostic useful when running many short
chains. It is calculated on superchains, which are groups of chains that
have been initialized at the same point.

Note that there is a slight difference in the calculation of Rhat and
nested Rhat, as nested Rhat is lower bounded by 1. This means that
nested Rhat with one chain per superchain will not be exactly equal to
basic Rhat (see Footnote 3 in Margossian et al. (2024)).

## References

Charles C. Margossian, Matthew D. Hoffman, Pavel Sountsov, Lionel
Riou-Durand, Aki Vehtari and Andrew Gelman (2024). Nested R-hat:
Assessing the convergence of Markov chain Monte Carlo when running many
short chains. *Bayesian Analysis*. doi:10.1214/24-BA1453

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
[`rhat_basic()`](https://mc-stan.org/posterior/dev/reference/rhat_basic.md),
[`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md)

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
rhat_nested(mu, superchain_ids = c(1, 1, 2, 2))
#> [1] 1.002846

d <- as_draws_rvars(example_draws("multi_normal"))
rhat_nested(d$Sigma, superchain_ids = c(1, 1, 2, 2))
#>          [,1]     [,2]     [,3]
#> [1,] 1.000511 1.001590 1.000154
#> [2,] 1.001590 1.001037 1.000071
#> [3,] 1.000154 1.000071 1.000469
```

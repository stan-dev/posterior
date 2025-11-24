# Pareto khat diagnostic

Estimate Pareto k value by fitting a Generalized Pareto Distribution to
one or two tails of x. This can be used to estimate the number of
fractional moments that is useful for convergence diagnostics. For
further details see Vehtari et al. (2024).

## Usage

``` r
pareto_khat(x, ...)

# Default S3 method
pareto_khat(
  x,
  tail = c("both", "right", "left"),
  r_eff = NULL,
  ndraws_tail = NULL,
  verbose = FALSE,
  are_log_weights = FALSE,
  ...
)

# S3 method for class 'rvar'
pareto_khat(x, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Arguments passed to individual methods (if applicable).

- tail:

  (string) The tail to diagnose/smooth:

  - `"right"`: diagnose/smooth only the right (upper) tail

  - `"left"`: diagnose/smooth only the left (lower) tail

  - `"both"`: diagnose/smooth both tails and return the maximum k-hat
    value

  The default is `"both"`.

- r_eff:

  (numeric) relative effective sample size estimate. If `r_eff` is NULL,
  it will be calculated assuming the draws are from MCMC. Default is
  NULL.

- ndraws_tail:

  (numeric) number of draws for the tail. If `ndraws_tail` is not
  specified, it will be calculated as ceiling(3 \* sqrt(length(x) /
  r_eff)) if length(x) \> 225 and length(x) / 5 otherwise (see Appendix
  H in Vehtari et al. (2024)).

- verbose:

  (logical) Should diagnostic messages be printed? If `TRUE`, messages
  related to Pareto diagnostics will be printed. Default is `FALSE`.

- are_log_weights:

  (logical) Are the draws log weights? Default is `FALSE`. If `TRUE`
  computation will take into account that the draws are log weights, and
  only right tail will be smoothed.

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

Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and Jonah Gabry
(2024). Pareto Smoothed Importance Sampling. *Journal of Machine
Learning Research*, 25(72):1-58.
[PDF](https://jmlr.org/papers/v25/19-556.html)

## See also

[`pareto_diags`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
for additional related diagnostics, and
[`pareto_smooth`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md)
for Pareto smoothed draws.

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
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md),
[`rhat_basic()`](https://mc-stan.org/posterior/dev/reference/rhat_basic.md),
[`rhat_nested()`](https://mc-stan.org/posterior/dev/reference/rhat_nested.md),
[`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md)

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
pareto_khat(mu)
#> [1] 0.1883631

d <- as_draws_rvars(example_draws("multi_normal"))
pareto_khat(d$Sigma)
#>            [,1]       [,2]        [,3]
#> [1,] 0.04795008 0.04397814  0.04538642
#> [2,] 0.04397814 0.08793028  0.07088579
#> [3,] 0.04538642 0.07088579 -0.08599429
```

# Pareto smoothing diagnostics

Compute diagnostics for Pareto smoothing the tail draws of x by
replacing tail draws by order statistics of a generalized Pareto
distribution fit to the tail(s).

## Usage

``` r
pareto_diags(x, ...)

# Default S3 method
pareto_diags(
  x,
  tail = c("both", "right", "left"),
  r_eff = NULL,
  ndraws_tail = NULL,
  verbose = FALSE,
  are_log_weights = FALSE,
  ...
)

# S3 method for class 'rvar'
pareto_diags(x, ...)

pareto_khat_threshold(x, ...)

# Default S3 method
pareto_khat_threshold(x, ...)

# S3 method for class 'rvar'
pareto_khat_threshold(x, ...)

pareto_min_ss(x, ...)

# Default S3 method
pareto_min_ss(x, ...)

# S3 method for class 'rvar'
pareto_min_ss(x, ...)

pareto_convergence_rate(x, ...)

# Default S3 method
pareto_convergence_rate(x, ...)

# S3 method for class 'rvar'
pareto_convergence_rate(x, ...)
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

List of Pareto smoothing diagnostics:

- `khat`: estimated Pareto k shape parameter,

- `min_ss`: minimum sample size for reliable Pareto smoothed estimate,

- `khat_threshold`: khat-threshold for reliable Pareto smoothed
  estimate,

- `convergence_rate`: Pareto smoothed estimate RMSE convergence rate.

## Details

When the fitted Generalized Pareto Distribution is used to smooth the
tail values and these smoothed values are used to compute expectations,
the following diagnostics can give further information about the
reliability of these estimates.

- `min_ss`: Minimum sample size for reliable Pareto smoothed estimate.
  If the actual sample size is greater than `min_ss`, then Pareto
  smoothed estimates can be considered reliable. If the actual sample
  size is lower than `min_ss`, increasing the sample size might result
  in more reliable estimates. For further details, see Section 3.2.3,
  Equation 11 in Vehtari et al. (2024).

- `khat_threshold`: Threshold below which k-hat values result in
  reliable Pareto smoothed estimates. The threshold is lower for smaller
  effective sample sizes. If k-hat is larger than the threshold,
  increasing the total sample size may improve reliability of estimates.
  For further details, see Section 3.2.4, Equation 13 in Vehtari et al.
  (2024).

- `convergence_rate`: Relative convergence rate compared to the central
  limit theorem. Applicable only if the actual sample size is
  sufficiently large (greater than `min_ss`). The convergence rate tells
  the rate at which the variance of an estimate reduces when the sample
  size is increased, compared to the central limit theorem convergence
  rate. See Appendix B in Vehtari et al. (2024).

## References

Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao and Jonah Gabry
(2024). Pareto Smoothed Importance Sampling. *Journal of Machine
Learning Research*, 25(72):1-58.
[PDF](https://jmlr.org/papers/v25/19-556.html)

## See also

[`pareto_khat`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
`pareto_min_ss`, `pareto_khat_threshold`, and `pareto_convergence_rate`
for individual diagnostics; and
[`pareto_smooth`](https://mc-stan.org/posterior/dev/reference/pareto_smooth.md)
for Pareto smoothing draws.

Other diagnostics:
[`ess_basic()`](https://mc-stan.org/posterior/dev/reference/ess_basic.md),
[`ess_bulk()`](https://mc-stan.org/posterior/dev/reference/ess_bulk.md),
[`ess_quantile()`](https://mc-stan.org/posterior/dev/reference/ess_quantile.md),
[`ess_sd()`](https://mc-stan.org/posterior/dev/reference/ess_sd.md),
[`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md),
[`mcse_mean()`](https://mc-stan.org/posterior/dev/reference/mcse_mean.md),
[`mcse_quantile()`](https://mc-stan.org/posterior/dev/reference/mcse_quantile.md),
[`mcse_sd()`](https://mc-stan.org/posterior/dev/reference/mcse_sd.md),
[`pareto_khat()`](https://mc-stan.org/posterior/dev/reference/pareto_khat.md),
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md),
[`rhat_basic()`](https://mc-stan.org/posterior/dev/reference/rhat_basic.md),
[`rhat_nested()`](https://mc-stan.org/posterior/dev/reference/rhat_nested.md),
[`rstar()`](https://mc-stan.org/posterior/dev/reference/rstar.md)

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
pareto_diags(mu)
#> $khat
#> [1] 0.1883631
#> 
#> $min_ss
#> [1] 17.06389
#> 
#> $khat_threshold
#> [1] 0.6156891
#> 
#> $convergence_rate
#> [1] 0.9872523
#> 

d <- as_draws_rvars(example_draws("multi_normal"))
pareto_diags(d$Sigma)
#> $khat
#>            [,1]       [,2]        [,3]
#> [1,] 0.04795008 0.04397814  0.04538642
#> [2,] 0.04397814 0.08793028  0.07088579
#> [3,] 0.04538642 0.07088579 -0.08599429
#> 
#> $min_ss
#>          [,1]     [,2]     [,3]
#> [1,] 11.22962 11.11735 11.15692
#> [2,] 11.11735 12.48554 11.92049
#> [3,] 11.15692 11.92049 10.00000
#> 
#> $khat_threshold
#>           [,1]      [,2]      [,3]
#> [1,] 0.6156891 0.6156891 0.6156891
#> [2,] 0.6156891 0.6156891 0.6156891
#> [3,] 0.6156891 0.6156891 0.6156891
#> 
#> $convergence_rate
#>           [,1]      [,2]      [,3]
#> [1,] 0.9984733 0.9986277 0.9985736
#> [2,] 0.9986277 0.9965542 0.9974598
#> [3,] 0.9985736 0.9974598 1.0000000
#> 
```

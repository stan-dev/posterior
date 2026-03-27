# Estimate parameters of the Generalized Pareto distribution

Given a sample \\x\\, Estimate the parameters \\k\\ and \\\sigma\\ of
the generalized Pareto distribution (GPD), assuming the location
parameter is 0. By default the fit uses a prior for \\k\\ (this is in
addition to the prior described by Zhang and Stephens, 2009), which will
stabilize estimates for very small sample sizes (and low effective
sample sizes in the case of MCMC samples). The weakly informative prior
is a Gaussian prior centered at 0.5 (see details in Vehtari et al.,
2024). This is used internally but is exported for use by other
packages.

## Usage

``` r
gpdfit(x, wip = TRUE, min_grid_pts = 30, sort_x = TRUE, weights = NULL)
```

## Arguments

- x:

  A numeric vector. The sample from which to estimate the parameters.

- wip:

  Logical indicating whether to adjust \\k\\ based on a weakly
  informative Gaussian prior centered on 0.5. Defaults to `TRUE`.

- min_grid_pts:

  The minimum number of grid points used in the fitting algorithm. The
  actual number used is `min_grid_pts + floor(sqrt(length(x)))`.

- sort_x:

  If `TRUE` (the default), the first step in the fitting algorithm is to
  sort the elements of `x`. If `x` is already sorted in ascending order
  then `sort_x` can be set to `FALSE` to skip the initial sorting step.

- weights:

  An optional numeric vector of positive weights the same length as `x`.
  If `NULL` (the default), all observations are weighted equally and the
  result is identical to the unweighted fit. Weights are normalized
  internally to sum to `length(x)`.

## Value

A named list with components `k` and `sigma`.

## Details

Here the parameter \\k\\ is the negative of \\k\\ in Zhang & Stephens
(2009).

## References

Zhang, J., and Stephens, M. A. (2009). A new and efficient estimation
method for the generalized Pareto distribution. *Technometrics* **51**,
316-325.

## See also

Other helper-functions:
[`ps_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/ps_convergence_rate.md),
[`ps_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/ps_khat_threshold.md),
[`ps_min_ss()`](https://mc-stan.org/posterior/dev/reference/ps_min_ss.md),
[`ps_tail_length()`](https://mc-stan.org/posterior/dev/reference/ps_tail_length.md)

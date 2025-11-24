# Pareto k-hat threshold

Given number of draws, computes khat threshold for reliable Pareto
smoothed estimate (to have small probability of large error). See
section 3.2.4, equation (13) of Vehtari et al. (2024). This function is
exported to be usable by other packages. For user-facing diagnostic
functions, see
[`pareto_khat_threshold`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
and
[`pareto_diags`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md).

## Usage

``` r
ps_khat_threshold(ndraws, ...)
```

## Arguments

- ndraws:

  number of draws

- ...:

  unused

## Value

threshold

## See also

Other helper-functions:
[`ps_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/ps_convergence_rate.md),
[`ps_min_ss()`](https://mc-stan.org/posterior/dev/reference/ps_min_ss.md),
[`ps_tail_length()`](https://mc-stan.org/posterior/dev/reference/ps_tail_length.md)

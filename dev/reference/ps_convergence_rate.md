# Pareto convergence rate

Given number of draws and scalar or array of k's, compute the relative
convergence rate of PSIS estimate RMSE. See Appendix B of Vehtari et al.
(2024). This function is exported to be usable by other packages. For
user-facing diagnostic functions, see
[`pareto_convergence_rate`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md)
and
[`pareto_diags`](https://mc-stan.org/posterior/dev/reference/pareto_diags.md).

## Usage

``` r
ps_convergence_rate(k, ndraws, ...)
```

## Arguments

- k:

  pareto-k values

- ndraws:

  number of draws

- ...:

  unused

## Value

convergence rate

## See also

Other helper-functions:
[`ps_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/ps_khat_threshold.md),
[`ps_min_ss()`](https://mc-stan.org/posterior/dev/reference/ps_min_ss.md),
[`ps_tail_length()`](https://mc-stan.org/posterior/dev/reference/ps_tail_length.md)

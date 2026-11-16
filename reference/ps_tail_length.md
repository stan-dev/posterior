# Pareto tail length

Calculate the tail length from the number of draws. The tail length is
`floor(3 * sqrt(ndraws))` if `ndraws > 225`, and `floor(ndraws / 5)`
otherwise. This differs from Appendix H in Vehtari et al. (2024), which
also divides by the relative efficiency `r_eff`. This function is used
internally and is exported to be available for other packages.

## Usage

``` r
ps_tail_length(ndraws, r_eff = NULL, ...)
```

## Arguments

- ndraws:

  number of draws

- r_eff:

  Ignored. A value other than `NULL` or 1 gives a warning.

- ...:

  unused

## Value

tail length

## See also

Other helper-functions:
[`gpdfit()`](https://mc-stan.org/posterior/reference/gpdfit.md),
[`ps_convergence_rate()`](https://mc-stan.org/posterior/reference/ps_convergence_rate.md),
[`ps_khat_threshold()`](https://mc-stan.org/posterior/reference/ps_khat_threshold.md),
[`ps_min_ss()`](https://mc-stan.org/posterior/reference/ps_min_ss.md)

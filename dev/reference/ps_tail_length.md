# Pareto tail length

Calculate the tail length from number of draws and relative efficiency
r_eff. See Appendix H in Vehtari et al. (2024). This function is used
internally and is exported to be available for other packages.

## Usage

``` r
ps_tail_length(ndraws, r_eff, ...)
```

## Arguments

- ndraws:

  number of draws

- r_eff:

  relative efficiency

- ...:

  unused

## Value

tail length

## See also

Other helper-functions:
[`ps_convergence_rate()`](https://mc-stan.org/posterior/dev/reference/ps_convergence_rate.md),
[`ps_khat_threshold()`](https://mc-stan.org/posterior/dev/reference/ps_khat_threshold.md),
[`ps_min_ss()`](https://mc-stan.org/posterior/dev/reference/ps_min_ss.md)

# Pareto smooth sorted tail of log ratios

Function to Pareto smooth a sorted tail of log ratios. Exported for
usage in other packages, not by users.

## Usage

``` r
ps_tail_sorted_log_ratios(x, cutoff, smooth_draws = TRUE, ...)
```

## Arguments

- x:

  Vector of log ratios in the tail, sorted in ascending order and
  shifted so that `exp(x)` does not overflow, e.g. by subtracting the
  maximum of all log ratios.

- cutoff:

  The largest log ratio not in the tail, on the same shifted scale as
  `x`. Tail values may be equal to it.

- smooth_draws:

  (logical) Should the tail be smoothed? If `FALSE`, only `k` is
  estimated and `tail` is `x` unchanged.

- ...:

  Arguments passed to
  [`gpdfit()`](https://mc-stan.org/posterior/reference/gpdfit.md).

## Value

A list with elements `tail`, the (smoothed) tail of log ratios, and `k`,
the estimated shape parameter. Unlike
[`ps_tail()`](https://mc-stan.org/posterior/reference/ps_tail.md), the
full vector of draws is not returned.

## See also

[`ps_tail`](https://mc-stan.org/posterior/reference/ps_tail.md) for the
general tail smoothing function.

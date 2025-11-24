# Summaries of random variables over array elements, within draws

Compute summaries of random variables over array elements and within
draws, producing a new random variable of length 1 (except in the case
of `rvar_range()`, see **Details**).

## Usage

``` r
rvar_mean(..., na.rm = FALSE)

rvar_median(..., na.rm = FALSE)

rvar_sum(..., na.rm = FALSE)

rvar_prod(..., na.rm = FALSE)

rvar_min(..., na.rm = FALSE)

rvar_max(..., na.rm = FALSE)

rvar_sd(..., na.rm = FALSE)

rvar_var(..., na.rm = FALSE)

rvar_mad(..., constant = 1.4826, na.rm = FALSE)

rvar_range(..., na.rm = FALSE)

rvar_quantile(..., probs, names = FALSE, na.rm = FALSE)

rvar_all(..., na.rm = FALSE)

rvar_any(..., na.rm = FALSE)
```

## Arguments

- ...:

  (rvar) One or more
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

- na.rm:

  (logical) Should `NA`s be removed from the input before summaries are
  computed? The default is `FALSE`.

- constant:

  (scalar real) For `rvar_mad()`, a scale factor for computing the
  median absolute deviation. See the details of
  [`stats::mad()`](https://rdrr.io/r/stats/mad.html) for the
  justification for the default value.

- probs:

  (numeric vector) For `rvar_quantile()`, probabilities in `[0, 1]`.

- names:

  (logical) For `rvar_quantile()`, if `TRUE`, the result has a `names`
  attribute.

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) of
length 1 (for [`range()`](https://rdrr.io/r/base/range.html), length 2;
for [`quantile()`](https://rdrr.io/r/stats/quantile.html), length equal
to `length(probs)`) with the same number of draws as the input rvar(s)
containing the summary statistic computed within each draw of the input
rvar(s).

## Details

These functions compute statistics within each draw of the random
variable. For summaries over draws (such as expectations), see
[rvar-summaries-over-draws](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md).

Each function defined here corresponds to the base function of the same
name without the `rvar_` prefix (e.g., `rvar_mean()` calls
[`mean()`](https://rdrr.io/r/base/mean.html) under the hood, etc).

## See also

[rvar-summaries-over-draws](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
for summary functions across draws (e.g. expectations).
[rvar-dist](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
for density, CDF, and quantile functions of random variables.

Other rvar-summaries:
[`rvar-summaries-over-draws`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md),
[`rvar_is_finite()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)

## Examples

``` r
set.seed(5678)
x = rvar_rng(rnorm, 4, mean = 1:4, sd = 2)

# These will give similar results to mean(1:4),
# median(1:4), sum(1:4), prod(1:4), etc
rvar_mean(x)
#> rvar<4000>[1] mean ± sd:
#> [1] 2.5 ± 1 
rvar_median(x)
#> rvar<4000>[1] mean ± sd:
#> [1] 2.5 ± 1.1 
rvar_sum(x)
#> rvar<4000>[1] mean ± sd:
#> [1] 9.9 ± 4 
rvar_prod(x)
#> rvar<4000>[1] mean ± sd:
#> [1] 23 ± 103 
rvar_range(x)
#> rvar<4000>[2] mean ± sd:
#> [1] 0.029 ± 1.5  4.935 ± 1.5 
rvar_quantile(x, probs = c(0.25, 0.5, 0.75), names = TRUE)
#> rvar<4000>[3] mean ± sd:
#>        25%        50%        75% 
#> 1.3 ± 1.2  2.5 ± 1.1  3.6 ± 1.2  
```

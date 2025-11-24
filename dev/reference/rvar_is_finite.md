# Special value predicates for random variables

Compute special value predicates (checking for finite / infinite values,
`NaN`, and `NA`) on all draws within a random variable, returning a
random variable.

## Usage

``` r
rvar_is_finite(x)

rvar_is_infinite(x)

rvar_is_nan(x)

rvar_is_na(x)
```

## Arguments

- x:

  (rvar) An
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Value

A logical [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
of the same length as the input.

## Details

These functions return a new
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) that is
the result of applying
[`is.finite()`](https://rdrr.io/r/base/is.finite.html),
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html), or
[`is.na()`](https://rdrr.io/r/base/NA.html) to every draw in the input
random variable.

## See also

[rvar-summaries-over-draws](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
for summary functions across draws, including implementations of
[`is.finite()`](https://rdrr.io/r/base/is.finite.html),
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html), and
[`is.na()`](https://rdrr.io/r/base/NA.html) for `rvar`s.

Other rvar-summaries:
[`rvar-summaries-over-draws`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md),
[`rvar-summaries-within-draws`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)

## Examples

``` r
x <- rvar(c(1, Inf, -Inf, NaN, NA))
x
#> rvar<5>[1] mean ± sd:
#> [1] NA ± NA 

rvar_is_finite(x)
#> rvar<5>[1] mean ± sd:
#> [1] 0.2 ± 0.45 
rvar_is_infinite(x)
#> rvar<5>[1] mean ± sd:
#> [1] 0.4 ± 0.55 
rvar_is_nan(x)
#> rvar<5>[1] mean ± sd:
#> [1] 0.2 ± 0.45 
rvar_is_na(x)
#> rvar<5>[1] mean ± sd:
#> [1] 0.4 ± 0.55 
```

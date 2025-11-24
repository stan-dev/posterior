# Summaries of random variables within array elements, over draws

Compute summaries within elements of an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) and over
draws of each element, producing an array of the same shape as the input
random variable (except in the case of
[`range()`](https://rdrr.io/r/base/range.html), see **Details**).

## Usage

``` r
E(x, ...)

# S3 method for class 'rvar'
mean(x, ...)

Pr(x, ...)

# Default S3 method
Pr(x, ...)

# S3 method for class 'logical'
Pr(x, ...)

# S3 method for class 'rvar'
Pr(x, ...)

# S3 method for class 'rvar'
median(x, ...)

# S3 method for class 'rvar'
min(x, ...)

# S3 method for class 'rvar'
max(x, ...)

# S3 method for class 'rvar'
sum(x, ...)

# S3 method for class 'rvar'
prod(x, ...)

# S3 method for class 'rvar'
all(x, ...)

# S3 method for class 'rvar'
any(x, ...)

# S3 method for class 'rvar'
Summary(...)

# S3 method for class 'rvar'
variance(x, ...)

var(x, ...)

# Default S3 method
var(x, ...)

# S3 method for class 'rvar'
var(x, ...)

sd(x, ...)

# Default S3 method
sd(x, ...)

# S3 method for class 'rvar'
sd(x, ...)

mad(x, ...)

# Default S3 method
mad(x, ...)

# S3 method for class 'rvar'
mad(x, ...)

# S3 method for class 'rvar_ordered'
mad(x, ...)

# S3 method for class 'rvar'
range(x, ...)

# S3 method for class 'rvar'
is.finite(x)

# S3 method for class 'rvar'
is.infinite(x)

# S3 method for class 'rvar'
is.nan(x)

# S3 method for class 'rvar'
is.na(x)
```

## Arguments

- x:

  (rvar) An
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Further arguments passed to underlying functions (e.g.,
  [`base::mean()`](https://rdrr.io/r/base/mean.html) or
  `base::median()`), such as `na.rm`.

## Value

A numeric or logical vector with the same dimensions as the given random
variable, where each entry in the vector is the mean, median, or
variance of the corresponding entry in `x`.

## Details

Summaries include expectations (`E()` or
[`mean()`](https://rdrr.io/r/base/mean.html)), probabilities (`Pr()`),
medians ([`median()`](https://rdrr.io/r/stats/median.html)), spread
(`var()`,
[`variance()`](https://pkg.mitchelloharawild.com/distributional/reference/variance.html),
`sd()`, `mad()`), sums and products
([`sum()`](https://rdrr.io/r/base/sum.html),
[`prod()`](https://rdrr.io/r/base/prod.html)), extrema and ranges
([`min()`](https://rdrr.io/r/base/Extremes.html),
[`max()`](https://rdrr.io/r/base/Extremes.html),
[`range()`](https://rdrr.io/r/base/range.html)), logical summaries
([`all()`](https://rdrr.io/r/base/all.html),
[`any()`](https://rdrr.io/r/base/any.html)), and special value
predicates ([`is.finite()`](https://rdrr.io/r/base/is.finite.html),
[`is.infinite()`](https://rdrr.io/r/base/is.finite.html),
[`is.nan()`](https://rdrr.io/r/base/is.finite.html),
[`is.na()`](https://rdrr.io/r/base/NA.html)).

Unless otherwise stated, these functions return a numeric array with the
same shape (same dimensions) as the input
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), `x`.

`range(x)` returns an array with dimensions `c(2, dim(x))`, where the
last dimension contains the minimum and maximum values.

`is.infinite(x)`, `is.nan(x)`, and `is.na(x)` return logical arrays,
where each element is `TRUE` if **any** draws in its corresponding
element in `x` match the predicate. Each elements in the result of
`is.finite(x)` is `TRUE` if **all** draws in the corresponding element
in `x` are finite.

Both `E()`, [`mean()`](https://rdrr.io/r/base/mean.html), and `Pr()`
return the means of each element in the input. `Pr()` additionally
checks that the provided
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) is a
logical variable (hence, taking its expectation results in a
probability).

For consistency, `E()` and `Pr()` are also defined for base arrays so
that they can be used as summary functions in
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md).

## See also

[rvar-summaries-within-draws](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md)
for summary functions within draws.
[rvar-dist](https://mc-stan.org/posterior/dev/reference/rvar-dist.md)
for density, CDF, and quantile functions of random variables.

Other rvar-summaries:
[`rvar-summaries-within-draws`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-within-draws.md),
[`rvar_is_finite()`](https://mc-stan.org/posterior/dev/reference/rvar_is_finite.md)

## Examples

``` r
set.seed(5678)
x = rvar_rng(rnorm, 4, mean = 1:4, sd = 2)

# These should all be ~= c(1, 2, 3, 4)
E(x)
#> [1] 1.004952 1.955997 3.011070 3.963980
mean(x)
#> [1] 1.004952 1.955997 3.011070 3.963980
median(x)
#> [1] 0.9557178 1.9170014 3.0024683 3.9274693

# This ...
Pr(x < 1.5)
#> [1] 0.59800 0.41100 0.22300 0.10775
# ... should be about the same as this:
pnorm(1.5, mean = 1:4, sd = 2)
#> [1] 0.5987063 0.4012937 0.2266274 0.1056498
```

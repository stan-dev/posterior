# Density, CDF, and quantile functions of random variables

The probability density function
([`density()`](https://rdrr.io/r/stats/density.html)), cumulative
distribution function
([`cdf()`](https://pkg.mitchelloharawild.com/distributional/reference/cdf.html)),
and quantile function / inverse CDF
([`quantile()`](https://rdrr.io/r/stats/quantile.html)) of an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Usage

``` r
# S3 method for class 'rvar'
density(x, at, ...)

# S3 method for class 'rvar_factor'
density(x, at, ...)

# S3 method for class 'rvar'
cdf(x, q, ...)

# S3 method for class 'rvar_factor'
cdf(x, q, ...)

# S3 method for class 'rvar_ordered'
cdf(x, q, ...)

# S3 method for class 'rvar'
quantile(x, probs, ...)

# S3 method for class 'rvar_factor'
quantile(x, probs, ...)

# S3 method for class 'rvar_ordered'
quantile(x, probs, ...)
```

## Arguments

- x:

  (rvar) An
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) object.

- ...:

  Additional arguments passed onto underlying methods:

  - For [`density()`](https://rdrr.io/r/stats/density.html), these are
    passed to
    [`stats::density()`](https://rdrr.io/r/stats/density.html).

  - For
    [`cdf()`](https://pkg.mitchelloharawild.com/distributional/reference/cdf.html),
    these are ignored.

  - For [`quantile()`](https://rdrr.io/r/stats/quantile.html), these are
    passed to
    [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- q, at:

  (numeric vector) One or more quantiles.

- probs:

  (numeric vector) One or more probabilities in `[0,1]`.

## Value

If `x` is a scalar
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), returns a
vector of the same length as the input (`q`, `at`, or `probs`)
containing values from the corresponding function of the given
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

If `x` has length greater than 1, returns an array with dimensions
`c(length(y), dim(x))` where `y` is `q`, `at`, or `probs`, where each
`result[i,...]` is the value of the corresponding function,`f(y[i])`,
for the corresponding cell in the input array, `x[...]`.

## Examples

``` r
set.seed(1234)
x = rvar(rnorm(100))

density(x, seq(-2, 2, length.out = 10))
#>  [1] 0.05258808 0.15648102 0.33299057 0.42878621 0.38792329 0.27729220
#>  [7] 0.20773876 0.14704714 0.10377342 0.07004217
cdf(x, seq(-2, 2, length.out = 10))
#>  [1] 0.02 0.04 0.15 0.34 0.54 0.68 0.81 0.89 0.92 0.96
quantile(x, ppoints(10))
#>  [1] -1.371156446 -1.107797288 -0.877775358 -0.574614497 -0.476119783
#>  [6] -0.187048613 -0.007696628  0.448815321  0.902987295  1.646284259

x2 = c(rvar(rnorm(100, mean = -0.5)), rvar(rnorm(100, mean = 0.5)))
density(x2, seq(-2, 2, length.out = 10))
#>             [,1]        [,2]
#>  [1,] 0.08835714 0.001630564
#>  [2,] 0.20312919 0.024267719
#>  [3,] 0.33416548 0.081561624
#>  [4,] 0.41908541 0.167585644
#>  [5,] 0.40299700 0.241650760
#>  [6,] 0.28262207 0.309646173
#>  [7,] 0.15825686 0.462297561
#>  [8,] 0.12739913 0.466521257
#>  [9,] 0.07744769 0.234440870
#> [10,] 0.02144551 0.109840500
cdf(x2, seq(-2, 2, length.out = 10))
#>       [,1] [,2]
#>  [1,] 0.07 0.01
#>  [2,] 0.11 0.01
#>  [3,] 0.25 0.03
#>  [4,] 0.43 0.08
#>  [5,] 0.63 0.16
#>  [6,] 0.79 0.31
#>  [7,] 0.86 0.43
#>  [8,] 0.91 0.69
#>  [9,] 0.97 0.87
#> [10,] 0.99 0.93
quantile(x2, ppoints(10))
#>             [,1]       [,2]
#>  [1,] -2.0034466 -0.6872996
#>  [2,] -1.3541896 -0.2346473
#>  [3,] -1.0276966  0.1308444
#>  [4,] -0.8207823  0.3500387
#>  [5,] -0.5789329  0.6861700
#>  [6,] -0.3647747  0.8192384
#>  [7,] -0.1451222  0.9367486
#>  [8,]  0.1230689  1.1749908
#>  [9,]  0.4371228  1.4569163
#> [10,]  1.1776442  2.1500961
```

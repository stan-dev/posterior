# Thin `draws` objects

Thin [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
objects to reduce their size and autocorrelation in the chains.

## Usage

``` r
thin_draws(x, thin = NULL, ...)

# S3 method for class 'draws'
thin_draws(x, thin = NULL, ...)

# S3 method for class 'rvar'
thin_draws(x, thin = NULL, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- thin:

  (positive numeric) The period for selecting draws. Must be between 1
  and the number of iterations. If the value is not an integer, the
  draws will be selected such that the number of draws returned is equal
  to round(ndraws(x) / thin). Intervals between selected draws will be
  either ceiling(thin) or floor(thin), such that the average interval
  will be close to the thin value. If `NULL`, it will be automatically
  calculated based on bulk and tail effective sample size as suggested
  by Säilynoja et al. (2022).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A `draws` object of the same class as `x`.

## References

Teemu Säilynoja, Paul-Christian Bürkner, and Aki Vehtari (2022).
Graphical test for discrete uniformity and its applications in
goodness-of-fit evaluation and multiple sample comparison. *Statistics
and Computing*. 32, 32. doi:10.1007/s11222-022-10090-6

## Examples

``` r
x <- example_draws()
niterations(x)
#> [1] 100

x <- thin_draws(x, thin = 5)
niterations(x)
#> [1] 20
```

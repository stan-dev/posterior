# Quantile function for the generalized Pareto distribution

Computes the quantile function for a generalized Pareto distribution
with location `mu`, scale `sigma`, and shape `k`.

## Usage

``` r
qgeneralized_pareto(
  p,
  mu = 0,
  sigma = 1,
  k = 0,
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- p:

  Numeric vector of probabilities.

- mu:

  Location parameter.

- sigma:

  Scale parameter (must be positive).

- k:

  Shape parameter.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are `P[X <= x]`.

- log.p:

  Logical; if `TRUE`, probabilities are given as `log(p)`.

## Value

A numeric vector of quantiles.

## Examples

``` r
qgeneralized_pareto(p = c(0.1, 0.5, 0.9), mu = 0, sigma = 1, k = 0.2)
#> [1] 0.1064784 0.7434918 2.9244660
```

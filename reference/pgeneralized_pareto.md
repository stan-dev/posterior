# Distribution function for the generalized Pareto distribution

Computes the cumulative distribution function (CDF) for a generalized
Pareto distribution with location `mu`, scale `sigma`, and shape `k`.

## Usage

``` r
pgeneralized_pareto(
  q,
  mu = 0,
  sigma = 1,
  k = 0,
  lower.tail = TRUE,
  log.p = FALSE
)
```

## Arguments

- q:

  Numeric vector of quantiles.

- mu:

  Location parameter.

- sigma:

  Scale parameter (must be positive).

- k:

  Shape parameter.

- lower.tail:

  Logical; if `TRUE` (default), probabilities are `P[X <= x]`.

- log.p:

  Logical; if `TRUE`, probabilities are returned as `log(p)`.

## Value

A numeric vector of probabilities.

## Examples

``` r
pgeneralized_pareto(q = c(1, 2, 5), mu = 0, sigma = 1, k = 0.2)
#> [1] 0.5981224 0.8140656 0.9687500
```

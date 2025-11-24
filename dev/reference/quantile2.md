# Compute Quantiles

Compute quantiles of a sample and return them in a format consistent
with other summary functions in the posterior package.

## Usage

``` r
quantile2(x, probs = c(0.05, 0.95), na.rm = FALSE, ...)

# Default S3 method
quantile2(x, probs = c(0.05, 0.95), na.rm = FALSE, names = TRUE, ...)

# S3 method for class 'rvar'
quantile2(x, probs = c(0.05, 0.95), na.rm = FALSE, names = TRUE, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- probs:

  (numeric vector) Probabilities in `[0, 1]`.

- na.rm:

  (logical) Should `NA` and `NaN` values be removed from `x` prior to
  computing quantiles? The default is `FALSE`.

- ...:

  Arguments passed to individual methods (if applicable) and then on to
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- names:

  (logical) Should the result have a `names` attribute? The default is
  `TRUE`, but use `FALSE` for improved speed if there are many values in
  `probs`.

## Value

A numeric vector of length `length(probs)`. If `names = TRUE`, it has a
[names](https://rdrr.io/r/base/names.html) attribute with names like
`"q5"`, `"q95"`, etc, based on the values of `probs`.

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
quantile2(mu)
#>         q5        q95 
#> -0.8536202  9.3873124 
```

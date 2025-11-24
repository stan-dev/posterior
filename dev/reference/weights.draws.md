# Extract Weights from Draws Objects

Extract weights from
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects,
with one weight per draw. See
[`weight_draws`](https://mc-stan.org/posterior/dev/reference/weight_draws.md)
for details how to add weights to
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects.

## Usage

``` r
# S3 method for class 'draws'
weights(object, log = FALSE, normalize = TRUE, ...)
```

## Arguments

- object:

  (draws) A
  [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
  object.

- log:

  (logical) Should the weights be returned on the log scale? Defaults to
  `FALSE`.

- normalize:

  (logical) Should the weights be normalized to sum to 1 on the standard
  scale? Defaults to `TRUE`.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A vector of weights, with one weight per draw.

## See also

[`weight_draws`](https://mc-stan.org/posterior/dev/reference/weight_draws.md),
[`resample_draws`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)

## Examples

``` r
x <- example_draws()

# sample some random weights for illustration
wts <- rexp(ndraws(x))
head(wts)
#> [1] 3.50844541 0.69823066 1.11631261 0.01682935 0.64546527 0.03515349

# add weights
x <- weight_draws(x, weights = wts)

# extract weights
head(weights(x)) # defaults to normalized weights
#> [1] 9.333062e-03 1.857412e-03 2.969582e-03 4.476894e-05 1.717048e-03
#> [6] 9.351427e-05
head(weights(x, normalize=FALSE)) # recover original weights
#> [1] 3.50844541 0.69823066 1.11631261 0.01682935 0.64546527 0.03515349
head(weights(x, log=TRUE)) # get normalized log-weights
#> [1]  -4.674192  -6.288571  -5.819334 -10.013996  -6.367149  -9.277397

# add weights which are already on the log scale
log_wts <- log(wts)
head(log_wts)
#> [1]  1.2551730 -0.3592058  0.1100309 -4.0846308 -0.4377839 -3.3480314

x <- weight_draws(x, weights = log_wts, log = TRUE)
# extract weights
head(weights(x))
#> [1] 9.333062e-03 1.857412e-03 2.969582e-03 4.476894e-05 1.717048e-03
#> [6] 9.351427e-05
head(weights(x, log=TRUE, normalize = FALSE)) # recover original log_wts
#> [1]  1.2551730 -0.3592058  0.1100309 -4.0846308 -0.4377839 -3.3480314

# add weights on log scale and Pareto smooth them
x <- weight_draws(x, weights = log_wts, log = TRUE, pareto_smooth = TRUE)
#> Pareto k-hat = -0.01.
```

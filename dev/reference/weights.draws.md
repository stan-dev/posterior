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
#> [1] 0.44924386 0.01132425 0.79627288 0.06250828 0.26065786 0.04660613

# add weights
x <- weight_draws(x, weights = wts)

# extract weights
head(weights(x)) # defaults to normalized weights
#> [1] 1.191764e-03 3.004123e-05 2.112371e-03 1.658234e-04 6.914792e-04
#> [6] 1.236378e-04
head(weights(x, normalize=FALSE)) # recover original weights
#> [1] 0.44924386 0.01132425 0.79627288 0.06250828 0.26065786 0.04660613
head(weights(x, log=TRUE)) # get normalized log-weights
#> [1]  -6.732320 -10.412940  -6.159944  -8.704587  -7.276678  -8.998154

# add weights which are already on the log scale
log_wts <- log(wts)
head(log_wts)
#> [1] -0.8001894 -4.4808090 -0.2278133 -2.7724563 -1.3445466 -3.0660231

x <- weight_draws(x, weights = log_wts, log = TRUE)
# extract weights
head(weights(x))
#> [1] 1.191764e-03 3.004123e-05 2.112371e-03 1.658234e-04 6.914792e-04
#> [6] 1.236378e-04
head(weights(x, log=TRUE, normalize = FALSE)) # recover original log_wts
#> [1] -0.8001894 -4.4808090 -0.2278133 -2.7724563 -1.3445466 -3.0660231

# add weights on log scale and Pareto smooth them
x <- weight_draws(x, weights = log_wts, log = TRUE, pareto_smooth = TRUE)
#> Pareto k-hat = 0.07.
```

# Weight `draws` objects

Add weights to
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects,
with one weight per draw, for use in subsequent weighting operations.
For reasons of numerical accuracy, weights are stored in the form of
unnormalized log-weights (in a variable called `.log_weight`). See
[`weights.draws()`](https://mc-stan.org/posterior/dev/reference/weights.draws.md)
for details how to extract weights from `draws` objects.

## Usage

``` r
weight_draws(x, weights, ...)

# S3 method for class 'draws_matrix'
weight_draws(x, weights, log = FALSE, pareto_smooth = FALSE, ...)

# S3 method for class 'draws_array'
weight_draws(x, weights, log = FALSE, pareto_smooth = FALSE, ...)

# S3 method for class 'draws_df'
weight_draws(x, weights, log = FALSE, pareto_smooth = FALSE, ...)

# S3 method for class 'draws_list'
weight_draws(x, weights, log = FALSE, pareto_smooth = FALSE, ...)

# S3 method for class 'draws_rvars'
weight_draws(x, weights, log = FALSE, pareto_smooth = FALSE, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- weights:

  (numeric vector) A vector of weights of length `ndraws(x)`. Weights
  will be internally stored on the log scale (in a variable called
  `.log_weight`) and will not be normalized, but normalized (non-log)
  weights can be returned via the
  [`weights.draws()`](https://mc-stan.org/posterior/dev/reference/weights.draws.md)
  method later.

- ...:

  Arguments passed to individual methods (if applicable).

- log:

  (logical) Are the weights passed already on the log scale? The default
  is `FALSE`, that is, expecting `weights` to be on the standard
  (non-log) scale.

- pareto_smooth:

  (logical) Should the weights be Pareto-smoothed? The default is
  `FALSE`.

## Value

A `draws` object of the same class as `x`.

## See also

[`weights.draws()`](https://mc-stan.org/posterior/dev/reference/weights.draws.md),
[`resample_draws()`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)

## Examples

``` r
x <- example_draws()

# sample some random weights for illustration
wts <- rexp(ndraws(x))
head(wts)
#> [1] 0.07943442 1.20975078 0.09003050 0.22634413 0.47107425 0.22054226

# add weights
x <- weight_draws(x, weights = wts)

# extract weights
head(weights(x)) # defaults to normalized weights
#> [1] 0.0002133569 0.0032493306 0.0002418175 0.0006079491 0.0012652821
#> [6] 0.0005923656
head(weights(x, normalize=FALSE)) # recover original weights
#> [1] 0.07943442 1.20975078 0.09003050 0.22634413 0.47107425 0.22054226
head(weights(x, log=TRUE)) # get normalized log-weights
#> [1] -8.452544 -5.729306 -8.327327 -7.405419 -6.672460 -7.431387

# add weights which are already on the log scale
log_wts <- log(wts)
head(log_wts)
#> [1] -2.5328235  0.1904144 -2.4076068 -1.4856988 -0.7527396 -1.5116659

x <- weight_draws(x, weights = log_wts, log = TRUE)
# extract weights
head(weights(x))
#> [1] 0.0002133569 0.0032493306 0.0002418175 0.0006079491 0.0012652821
#> [6] 0.0005923656
head(weights(x, log=TRUE, normalize = FALSE)) # recover original log_wts
#> [1] -2.5328235  0.1904144 -2.4076068 -1.4856988 -0.7527396 -1.5116659

# add weights on log scale and Pareto smooth them
x <- weight_draws(x, weights = log_wts, log = TRUE, pareto_smooth = TRUE)
#> Pareto k-hat = 0.03.
```

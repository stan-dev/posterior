# Probability integral transform

Probability integral transform (PIT). LOO-PIT is given by a weighted
sample.

## Usage

``` r
pit(x, y, ...)

# Default S3 method
pit(x, y, weights = NULL, log = FALSE, ...)

# S3 method for class 'draws_matrix'
pit(x, y, weights = NULL, log = FALSE, ...)

# S3 method for class 'rvar'
pit(x, y, weights = NULL, log = FALSE, ...)
```

## Arguments

- x:

  (draws) A
  [`draws_matrix`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
  object or one coercible to a `draws_matrix` object, or an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) object.

- y:

  (observations) A 1D vector, or an array of dim(x), if x is `rvar`.
  Each element of `y` corresponds to a variable in `x`.

- ...:

  Arguments passed to individual methods (if applicable).

- weights:

  A matrix of weights for each draw and variable. `weights` should have
  one column per variable in `x`, and `ndraws(x)` rows.

- log:

  (logical) Are the weights passed already on the log scale? The default
  is `FALSE`, that is, expecting `weights` to be on the standard
  (non-log) scale.

## Value

A numeric vector of length `length(y)` containing the PIT values, or an
array of shape `dim(y)`, if `x` is an `rvar`.

## Details

The `pit()` function computes the probability integral transform of `y`
using the empirical cumulative distribution computed from the samples in
`x`. For continuous valued `y` and `x`, the PIT for the elements of `y`
is computed as the empirical cumulative distribution value:

    PIT(y_i) = Pr(x_i < y_i),

where x_i, is the corresponding set of draws in `x`. For `draws`
objects, this corresponds to the draws of the *i*th variable, and for
`rvar` the elements of `y` and `x` are matched.

The draws in `x` can further be provided (log-)weights in

If `y` and `x` are discrete, randomisation is used to obtain continuous
PIT values. (see, e.g., Czado, C., Gneiting, T., Held, L.: Predictive
model assessment for count data. Biometrics 65(4), 1254–1261 (2009).)

## Examples

``` r
# PIT for a draws object
x <- example_draws()
# Create a vector of observations
y <- rnorm(nvariables(x), 5, 5)
pit(x, y)
#>       mu      tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6] 
#>   0.1150   0.1575   0.3200   0.9675   0.8300   0.8675   0.5875   0.1100 
#> theta[7] theta[8] 
#>   0.7025   0.4475 

# Compute weighted PIT (for example LOO-PIT)
weights <- matrix(runif(length(x)), ncol = nvariables(x))

pit(x, y, weights)
#>        mu       tau  theta[1]  theta[2]  theta[3]  theta[4]  theta[5]  theta[6] 
#> 0.1188946 0.1573926 0.3205254 0.9633941 0.8292876 0.8611088 0.5991041 0.1101363 
#>  theta[7]  theta[8] 
#> 0.6934052 0.4586079 

# PIT for an rvar
x <- rvar(example_draws())
# Create an array of observations with the same dimensions as x.
y_arr <- array(rnorm(length(x), 5, 5), dim = dim(x))
pit(x, y_arr)
#>      variable
#> chain   mu  tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6] theta[7]
#>     1 0.97 0.00     0.31     0.32     0.71     0.74     0.09     0.42     0.65
#>     2 0.45 0.96     0.12     0.10     0.56     0.64     0.95     0.33     0.18
#>     3 0.42 1.00     0.72     0.87     0.93     0.32     0.23     0.84     0.00
#>     4 0.02 0.46     0.46     0.52     0.51     0.27     0.17     0.63     0.81
#>      variable
#> chain theta[8]
#>     1     0.19
#>     2     0.83
#>     3     0.71
#>     4     0.12
```

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
#>   0.9600   0.8825   0.2200   0.2800   0.4100   0.4800   0.3975   0.9650 
#> theta[7] theta[8] 
#>   0.0775   0.6025 

# Compute weighted PIT (for example LOO-PIT)
weights <- matrix(runif(length(x)), ncol = nvariables(x))

pit(x, y, weights)
#>         mu        tau   theta[1]   theta[2]   theta[3]   theta[4]   theta[5] 
#> 0.96092124 0.88834727 0.23164173 0.30290652 0.39153378 0.47201908 0.42623629 
#>   theta[6]   theta[7]   theta[8] 
#> 0.97574934 0.07874259 0.60007608 

# PIT for an rvar
x <- rvar(example_draws())
# Create an array of observations with the same dimensions as x.
y_arr <- array(rnorm(length(x), 5, 5), dim = dim(x))
pit(x, y_arr)
#>      variable
#> chain   mu  tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6] theta[7]
#>     1 0.01 0.44     0.65     0.13     0.57     0.40     0.45     0.49     0.24
#>     2 0.99 0.42     0.27     0.88     0.41     0.81     0.39     0.23     0.07
#>     3 0.03 0.88     0.00     0.70     0.38     0.50     0.66     0.89     0.90
#>     4 0.03 0.72     0.80     0.03     0.24     0.15     0.13     0.53     0.49
#>      variable
#> chain theta[8]
#>     1     0.64
#>     2     0.30
#>     3     0.55
#>     4     0.20
```

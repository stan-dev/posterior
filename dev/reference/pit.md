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
using the empirical cumulative distribution computed from the draws in
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
#>   0.7200   0.9250   0.6900   0.0125   0.0825   0.9650   0.1900   0.2000 
#> theta[7] theta[8] 
#>   0.0650   0.2850 

# Compute weighted PIT (for example LOO-PIT)
weights <- matrix(runif(length(x)), ncol = nvariables(x))

pit(x, y, weights)
#>         mu        tau   theta[1]   theta[2]   theta[3]   theta[4]   theta[5] 
#> 0.73168924 0.92422631 0.67537851 0.01551178 0.07374251 0.96731879 0.18392838 
#>   theta[6]   theta[7]   theta[8] 
#> 0.18079978 0.05075895 0.30773529 

# PIT for an rvar
x <- rvar(example_draws())
# Create an array of observations with the same dimensions as x.
y_arr <- array(rnorm(length(x), 5, 5), dim = dim(x))
pit(x, y_arr)
#>      variable
#> chain   mu  tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6] theta[7]
#>     1 0.93 0.97     0.74     0.26     0.20     0.86     0.11     0.64     0.21
#>     2 0.51 0.78     0.39     0.28     0.19     0.52     0.87     0.14     0.12
#>     3 0.40 0.78     0.58     0.00     0.48     0.82     0.27     0.60     0.37
#>     4 0.03 0.70     0.55     0.92     0.38     0.27     0.94     0.40     0.81
#>      variable
#> chain theta[8]
#>     1     0.50
#>     2     0.24
#>     3     0.37
#>     4     0.34
```

# Print `draws_matrix` objects

Pretty printing for
[`draws_matrix`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
objects.

## Usage

``` r
# S3 method for class 'draws_matrix'
print(
  x,
  digits = 2,
  max_draws = getOption("posterior.max_draws", 10),
  max_variables = getOption("posterior.max_variables", 8),
  reserved = FALSE,
  ...
)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- digits:

  (nonnegative integer) The minimum number of significant digits to
  print. If `NULL`, defaults to `getOption("posterior.digits", 2)`.

- max_draws:

  (positive integer) The maximum number of draws to print. Can be
  controlled globally via the `"posterior.max_draws"`
  [option](https://rdrr.io/r/base/options.html).

- max_variables:

  (positive integer) The maximum number of variables to print. Can be
  controlled globally via the `"posterior.max_variables"`
  [option](https://rdrr.io/r/base/options.html).

- reserved:

  (logical) Should reserved variables be included in the output?
  Defaults to `FALSE`. See
  [`reserved_variables`](https://mc-stan.org/posterior/dev/reference/reserved_variables.md)
  for an overview of currently reserved variable names.

- ...:

  Further arguments passed to the underlying
  [`print()`](https://rdrr.io/r/base/print.html) methods.

## Value

A `draws` object of the same class as `x`.

## Examples

``` r
x <- as_draws_matrix(example_draws())
print(x)
#> # A draws_matrix: 100 iterations, 4 chains, and 10 variables
#>     variable
#> draw   mu tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6]
#>   1  2.01 2.8     3.96    0.271    -0.74      2.1    0.923      1.7
#>   2  1.46 7.0     0.12   -0.069     0.95      7.3   -0.062     11.3
#>   3  5.81 9.7    21.25   14.931     1.83      1.4    0.531      7.2
#>   4  6.85 4.8    14.70    8.586     2.67      4.4    4.758      8.1
#>   5  1.81 2.8     5.96    1.156     3.11      2.0    0.769      4.7
#>   6  3.84 4.1     5.76    9.909    -1.00      5.3    5.889     -1.7
#>   7  5.47 4.0     4.03    4.151    10.15      6.6    3.741     -2.2
#>   8  1.20 1.5    -0.28    1.846     0.47      4.3    1.467      3.3
#>   9  0.15 3.9     1.81    0.661     0.86      4.5   -1.025      1.1
#>   10 7.17 1.8     6.08    8.102     7.68      5.6    7.106      8.5
#> # ... with 390 more draws, and 2 more variables
```

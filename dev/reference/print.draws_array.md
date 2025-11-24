# Print `draws_array` objects

Pretty printing for
[`draws_array`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
objects.

## Usage

``` r
# S3 method for class 'draws_array'
print(
  x,
  digits = 2,
  max_iterations = getOption("posterior.max_iterations", 5),
  max_chains = getOption("posterior.max_chains", 8),
  max_variables = getOption("posterior.max_variables", 4),
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

- max_iterations:

  (positive integer) The maximum number of iterations to print. Can be
  controlled globally via the `"posterior.max_iterations"`
  [option](https://rdrr.io/r/base/options.html).

- max_chains:

  (positive integer) The maximum number of chains to print. Can be
  controlled globally via the `"posterior.max_chains"`
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
x <- as_draws_array(example_draws())
print(x)
#> # A draws_array: 100 iterations, 4 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration   1    2     3   4
#>         1 2.0  3.0  1.79 6.5
#>         2 1.5  8.2  5.99 9.1
#>         3 5.8 -1.2  2.56 0.2
#>         4 6.8 10.9  2.79 3.7
#>         5 1.8  9.8 -0.03 5.5
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration   1    2    3   4
#>         1 2.8 2.80  8.7 3.8
#>         2 7.0 2.76  2.9 6.8
#>         3 9.7 0.57  8.4 5.3
#>         4 4.8 2.45  4.4 1.6
#>         5 2.8 2.80 11.0 3.0
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration     1     2    3     4
#>         1  3.96  6.26 13.3  5.78
#>         2  0.12  9.32  6.3  2.09
#>         3 21.25 -0.97 10.6 15.72
#>         4 14.70 12.45  5.4  2.69
#>         5  5.96  9.75  8.2 -0.91
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration      1    2   3   4
#>         1  0.271  1.0 2.1 5.0
#>         2 -0.069  9.4 7.3 8.2
#>         3 14.931 -1.2 5.7 6.0
#>         4  8.586 12.5 2.8 2.7
#>         5  1.156 11.9 3.2 3.2
#> 
#> # ... with 95 more iterations, and 6 more variables
```

# Print `draws_list` objects

Pretty printing for
[`draws_list`](https://mc-stan.org/posterior/dev/reference/draws_list.md)
objects.

## Usage

``` r
# S3 method for class 'draws_list'
print(
  x,
  digits = 2,
  max_iterations = getOption("posterior.max_iterations", 10),
  max_chains = getOption("posterior.max_chains", 2),
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
x <- as_draws_list(example_draws())
print(x)
#> # A draws_list: 100 iterations, 4 chains, and 10 variables
#> 
#> [chain = 1]
#> $mu
#>  [1] 2.01 1.46 5.81 6.85 1.81 3.84 5.47 1.20 0.15 7.17
#> 
#> $tau
#>  [1] 2.8 7.0 9.7 4.8 2.8 4.1 4.0 1.5 3.9 1.8
#> 
#> $`theta[1]`
#>  [1]  3.96  0.12 21.25 14.70  5.96  5.76  4.03 -0.28  1.81  6.08
#> 
#> $`theta[2]`
#>  [1]  0.271 -0.069 14.931  8.586  1.156  9.909  4.151  1.846  0.661  8.102
#> 
#> 
#> [chain = 2]
#> $mu
#>  [1]   2.99   8.17  -1.15  10.93   9.82 -10.90  -9.26   1.79   5.35   0.87
#> 
#> $tau
#>  [1] 2.80 2.76 0.57 2.45 2.80 6.08 9.33 6.81 2.82 6.69
#> 
#> $`theta[1]`
#>  [1]  6.26  9.32 -0.97 12.45  9.75  2.56 11.92  9.89  4.31  9.26
#> 
#> $`theta[2]`
#>  [1]  1.0  9.4 -1.2 12.5 11.9 -8.8 -6.1 11.6  2.8  8.4
#> 
#> # ... with 90 more iterations, and 2 more chains, and 6 more variables
```

# Repair indices of `draws` objects

Repair indices of `draws` objects so that iterations, chains, and draws
are continuously and consistently numbered.

## Usage

``` r
repair_draws(x, order = TRUE, ...)

# S3 method for class 'draws_matrix'
repair_draws(x, order = TRUE, ...)

# S3 method for class 'draws_array'
repair_draws(x, order = TRUE, ...)

# S3 method for class 'draws_df'
repair_draws(x, order = TRUE, ...)

# S3 method for class 'draws_list'
repair_draws(x, order = TRUE, ...)

# S3 method for class 'draws_rvars'
repair_draws(x, order = TRUE, ...)

# S3 method for class 'rvar'
repair_draws(x, order = TRUE, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- order:

  (logical) Should draws be ordered (via
  [`order_draws()`](https://mc-stan.org/posterior/dev/reference/order_draws.md))
  before repairing indices? Defaults to `TRUE`.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A `draws` object of the same class as `x`.

## See also

[`order_draws()`](https://mc-stan.org/posterior/dev/reference/order_draws.md)

## Examples

``` r
x <- as_draws_array(example_draws())
(x <- x[10:5, 3:4, ])
#> # A draws_array: 6 iterations, 2 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration   3     4
#>        10 8.9  3.81
#>        9  8.9  0.88
#>        8  3.5  4.90
#>        7  3.7 11.82
#>        6  1.1  2.38
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration    3    4
#>        10 5.96  2.7
#>        9  5.96 15.8
#>        8  0.52  3.1
#>        7  1.68  4.3
#>        6  2.70  2.3
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration    3     4
#>        10 13.1  7.52
#>        9  13.1  9.07
#>        8   3.7  1.50
#>        7   5.2 18.87
#>        6   5.0  0.59
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration   3    4
#>        10 4.7  4.3
#>        9  4.7 11.6
#>        8  4.1  6.1
#>        7  4.1 13.0
#>        6  4.3  1.1
#> 
#> # ... with 1 more iterations, and 6 more variables
repair_draws(x)
#> # A draws_array: 6 iterations, 2 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration     1     2
#>         1 -0.03  5.48
#>         2  1.06  2.38
#>         3  3.67 11.82
#>         4  3.51  4.90
#>         5  8.85  0.88
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration     1    2
#>         1 11.03  3.0
#>         2  2.70  2.3
#>         3  1.68  4.3
#>         4  0.52  3.1
#>         5  5.96 15.8
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration    1     2
#>         1  8.2 -0.91
#>         2  5.0  0.59
#>         3  5.2 18.87
#>         4  3.7  1.50
#>         5 13.1  9.07
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration   1    2
#>         1 3.2  3.2
#>         2 4.3  1.1
#>         3 4.1 13.0
#>         4 4.1  6.1
#>         5 4.7 11.6
#> 
#> # ... with 1 more iterations, and 6 more variables
```

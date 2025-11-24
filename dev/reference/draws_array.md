# The `draws_array` format

The `as_draws_array()` methods convert objects to the `draws_array`
format. The `draws_array()` function creates an object of the
`draws_array` format based on a set of numeric vectors. See **Details**.

## Usage

``` r
as_draws_array(x, ...)

# Default S3 method
as_draws_array(x, ...)

# S3 method for class 'draws_array'
as_draws_array(x, ...)

# S3 method for class 'draws_matrix'
as_draws_array(x, ...)

# S3 method for class 'draws_df'
as_draws_array(x, ...)

# S3 method for class 'draws_list'
as_draws_array(x, ...)

# S3 method for class 'draws_rvars'
as_draws_array(x, ...)

# S3 method for class 'mcmc'
as_draws_array(x, ...)

# S3 method for class 'mcmc.list'
as_draws_array(x, ...)

draws_array(..., .nchains = 1)

is_draws_array(x)
```

## Arguments

- x:

  An object to convert to a `draws_array` object.

- ...:

  For `as_draws_array()`: Arguments passed to individual methods (if
  applicable). For `draws_array()`: Named arguments containing numeric
  vectors each defining a separate variable.

- .nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

A `draws_array` object, which has classes
`c("draws_array", "draws", "array")`.

## Details

Objects of class `"draws_array"` are 3-D arrays with dimensions
`"iteration"`, `"chain"`, and `"variable"`. See **Examples**.

## See also

Other formats:
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md),
[`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
[`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md),
[`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)

## Examples

``` r
x1 <- as_draws_array(example_draws())
class(x1)
#> [1] "draws_array" "draws"       "array"      
print(x1)
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
str(x1)
#>  'draws_array' num [1:100, 1:4, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:100] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...

x2 <- draws_array(a = rnorm(10), b = rnorm(10), c = 1)
class(x2)
#> [1] "draws_array" "draws"       "array"      
print(x2)
#> # A draws_array: 10 iterations, 1 chains, and 3 variables
#> , , variable = a
#> 
#>          chain
#> iteration       1
#>         1  0.1891
#>         2  0.7187
#>         3  0.0078
#>         4  2.4915
#>         5 -0.1703
#> 
#> , , variable = b
#> 
#>          chain
#> iteration      1
#>         1 -1.529
#>         2  1.389
#>         3  0.054
#>         4 -0.062
#>         5 -1.014
#> 
#> , , variable = c
#> 
#>          chain
#> iteration 1
#>         1 1
#>         2 1
#>         3 1
#>         4 1
#>         5 1
#> 
#> # ... with 5 more iterations
str(x2)
#>  'draws_array' num [1:10, 1, 1:3] 0.18907 0.71868 0.00777 2.49145 -0.17028 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:10] "1" "2" "3" "4" ...
#>   ..$ chain    : chr "1"
#>   ..$ variable : chr [1:3] "a" "b" "c"
```

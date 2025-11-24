# The `draws_list` format

The `as_draws_list()` methods convert objects to the `draws_list`
format. The `draws_list()` function creates an object of the
`draws_list` format based on a set of numeric vectors. See **Details**.

## Usage

``` r
as_draws_list(x, ...)

# Default S3 method
as_draws_list(x, ...)

# S3 method for class 'draws_list'
as_draws_list(x, ...)

# S3 method for class 'draws_matrix'
as_draws_list(x, ...)

# S3 method for class 'draws_array'
as_draws_list(x, ...)

# S3 method for class 'draws_df'
as_draws_list(x, ...)

# S3 method for class 'draws_rvars'
as_draws_list(x, ...)

# S3 method for class 'mcmc'
as_draws_list(x, ...)

# S3 method for class 'mcmc.list'
as_draws_list(x, ...)

draws_list(..., .nchains = 1)

is_draws_list(x)
```

## Arguments

- x:

  An object to convert to a `draws_list` object.

- ...:

  For `as_draws_list()`: Arguments passed to individual methods (if
  applicable). For `draws_list()`: Named arguments containing numeric
  vectors each defining a separate variable.

- .nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

A `draws_list` object, which has classes
`c("draws_list", "draws", "list")`.

## Details

Objects of class `"draws_list"` are lists with one element per MCMC
chain. Each of these elements is itself a named list of numeric vectors
with one vector per variable. The length of each vector is equal to the
number of saved iterations per chain. See **Examples**.

## See also

Other formats:
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md),
[`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md),
[`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md),
[`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)

## Examples

``` r
x1 <- as_draws_list(example_draws())
class(x1)
#> [1] "draws_list" "draws"      "list"      
print(x1)
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
str(x1)
#> List of 4
#>  $ 1:List of 10
#>   ..$ mu      : num [1:100] 2.01 1.46 5.81 6.85 1.81 ...
#>   ..$ tau     : num [1:100] 2.77 6.98 9.68 4.79 2.85 ...
#>   ..$ theta[1]: num [1:100] 3.962 0.124 21.251 14.7 5.96 ...
#>   ..$ theta[2]: num [1:100] 0.271 -0.069 14.931 8.586 1.156 ...
#>   ..$ theta[3]: num [1:100] -0.743 0.952 1.829 2.675 3.109 ...
#>   ..$ theta[4]: num [1:100] 2.1 7.28 1.38 4.39 1.99 ...
#>   ..$ theta[5]: num [1:100] 0.923 -0.062 0.531 4.758 0.769 ...
#>   ..$ theta[6]: num [1:100] 1.65 11.26 7.16 8.1 4.66 ...
#>   ..$ theta[7]: num [1:100] 3.32 9.62 14.8 9.49 1.21 ...
#>   ..$ theta[8]: num [1:100] 4.85 -8.64 -1.74 5.28 -4.54 ...
#>  $ 2:List of 10
#>   ..$ mu      : num [1:100] 2.99 8.17 -1.15 10.93 9.82 ...
#>   ..$ tau     : num [1:100] 2.804 2.757 0.569 2.453 2.798 ...
#>   ..$ theta[1]: num [1:100] 6.259 9.321 -0.972 12.455 9.751 ...
#>   ..$ theta[2]: num [1:100] 1.03 9.36 -1.19 12.53 11.94 ...
#>   ..$ theta[3]: num [1:100] 0.218 9.685 -1.373 11.155 12.718 ...
#>   ..$ theta[4]: num [1:100] -0.734 8.11 -0.794 10.476 9.698 ...
#>   ..$ theta[5]: num [1:100] 4.06 6.72 -0.787 10.595 9.946 ...
#>   ..$ theta[6]: num [1:100] 3.34 7.4 -1.18 10.51 8.35 ...
#>   ..$ theta[7]: num [1:100] 3.15 11.39 -1.43 15.54 12.91 ...
#>   ..$ theta[8]: num [1:100] -0.443 12.359 -1.775 14.45 15.171 ...
#>  $ 3:List of 10
#>   ..$ mu      : num [1:100] 1.7944 5.9864 2.5572 2.7944 -0.0296 ...
#>   ..$ tau     : num [1:100] 8.72 2.91 8.41 4.39 11.03 ...
#>   ..$ theta[1]: num [1:100] 13.32 6.31 10.56 5.37 8.2 ...
#>   ..$ theta[2]: num [1:100] 2.11 7.31 5.7 2.8 3.15 ...
#>   ..$ theta[3]: num [1:100] 1.38 4.11 -8.27 -10.77 -27.78 ...
#>   ..$ theta[4]: num [1:100] 7.82 4.7 6.8 6.8 12.32 ...
#>   ..$ theta[5]: num [1:100] -1.5 6.96 -3.15 -4.81 -14.91 ...
#>   ..$ theta[6]: num [1:100] 12.744 0.877 5.435 3.67 -0.246 ...
#>   ..$ theta[7]: num [1:100] 9.37 6.11 15.37 3.17 7.81 ...
#>   ..$ theta[8]: num [1:100] 11.98 2.26 4.27 2.91 3.52 ...
#>  $ 4:List of 10
#>   ..$ mu      : num [1:100] 6.459 9.145 0.203 3.693 5.48 ...
#>   ..$ tau     : num [1:100] 3.77 6.84 5.32 1.55 3 ...
#>   ..$ theta[1]: num [1:100] 5.776 2.086 15.719 2.689 -0.906 ...
#>   ..$ theta[2]: num [1:100] 4.96 8.19 6.04 2.67 3.19 ...
#>   ..$ theta[3]: num [1:100] 5.69 3.47 3.13 3.16 2.55 ...
#>   ..$ theta[4]: num [1:100] 4.84 7.78 6.06 7.92 11.27 ...
#>   ..$ theta[5]: num [1:100] 2.74 6.15 3.73 3.34 3.98 ...
#>   ..$ theta[6]: num [1:100] 0.607 1.227 1.538 0.26 -1.121 ...
#>   ..$ theta[7]: num [1:100] 9.54 10.12 7.11 2.11 7.31 ...
#>   ..$ theta[8]: num [1:100] 3.252 4.728 0.182 4.755 7.523 ...
#>  - attr(*, "class")= chr [1:3] "draws_list" "draws" "list"

x2 <- draws_list(a = rnorm(10), b = rnorm(10), c = 1)
class(x2)
#> [1] "draws_list" "draws"      "list"      
print(x2)
#> # A draws_list: 10 iterations, 1 chains, and 3 variables
#> 
#> [chain = 1]
#> $a
#>  [1]  1.121  0.567  0.607  1.594 -0.174  1.730 -0.292  0.921  0.469 -0.089
#> 
#> $b
#>  [1] -0.456 -0.245 -0.995  0.260  0.434  2.030 -0.760 -0.055 -0.941 -0.716
#> 
#> $c
#>  [1] 1 1 1 1 1 1 1 1 1 1
#> 
str(x2)
#> List of 1
#>  $ 1:List of 3
#>   ..$ a: num [1:10] 1.121 0.567 0.607 1.594 -0.174 ...
#>   ..$ b: num [1:10] -0.456 -0.245 -0.995 0.26 0.434 ...
#>   ..$ c: num [1:10] 1 1 1 1 1 1 1 1 1 1
#>  - attr(*, "class")= chr [1:3] "draws_list" "draws" "list"
```

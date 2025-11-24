# The `draws_matrix` format

The `as_draws_matrix()` methods convert objects to the `draws_matrix`
format. The `draws_matrix()` function creates an object of the
`draws_matrix` format based on a set of numeric vectors. See
**Details**.

## Usage

``` r
as_draws_matrix(x, ...)

# Default S3 method
as_draws_matrix(x, ...)

# S3 method for class 'draws_matrix'
as_draws_matrix(x, ...)

# S3 method for class 'draws_array'
as_draws_matrix(x, ...)

# S3 method for class 'draws_df'
as_draws_matrix(x, ...)

# S3 method for class 'draws_list'
as_draws_matrix(x, ...)

# S3 method for class 'draws_rvars'
as_draws_matrix(x, ...)

# S3 method for class 'mcmc'
as_draws_matrix(x, ...)

# S3 method for class 'mcmc.list'
as_draws_matrix(x, ...)

draws_matrix(..., .nchains = 1)

is_draws_matrix(x)
```

## Arguments

- x:

  An object to convert to a `draws_matrix` object.

- ...:

  For `as_draws_matrix()`: Arguments passed to individual methods (if
  applicable). For `draws_matrix()`: Named arguments containing numeric
  vectors each defining a separate variable.

- .nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

A `draws_matrix` object, which has classes
`c("draws_matrix", "draws", "matrix")`.

## Details

Objects of class `"draws_matrix"` are matrices (2-D arrays) with
dimensions `"draw"` and `"variable"`. See **Examples**.

## See also

Other formats:
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md),
[`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md),
[`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
[`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
[`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)

## Examples

``` r
x1 <- as_draws_matrix(example_draws())
class(x1)
#> [1] "draws_matrix" "draws"        "matrix"      
print(x1)
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
str(x1)
#>  'draws_matrix' num [1:400, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:400] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...
#>  - attr(*, "nchains")= int 4

x2 <- draws_matrix(a = rnorm(10), b = rnorm(10), c = 1)
class(x2)
#> [1] "draws_matrix" "draws"        "matrix"      
print(x2)
#> # A draws_matrix: 10 iterations, 1 chains, and 3 variables
#>     variable
#> draw     a     b c
#>   1  -0.29 -2.11 1
#>   2   2.17  0.87 1
#>   3   1.02 -0.84 1
#>   4   0.80  0.82 1
#>   5  -0.81  0.49 1
#>   6   1.88  0.42 1
#>   7   2.01  0.65 1
#>   8  -0.84  0.79 1
#>   9   0.21  0.36 1
#>   10  0.25 -0.87 1
str(x2)
#>  'draws_matrix' num [1:10, 1:3] -0.291 2.174 1.018 0.803 -0.805 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:10] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:3] "a" "b" "c"
#>  - attr(*, "nchains")= int 1
```

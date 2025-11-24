# The `draws_df` format

The `as_draws_df()` methods convert objects to the `draws_df` format.
The `draws_df()` function creates an object of the `draws_df` format
based on a set of numeric vectors. See **Details**.

## Usage

``` r
as_draws_df(x, ...)

# Default S3 method
as_draws_df(x, ...)

# S3 method for class 'data.frame'
as_draws_df(x, ...)

# S3 method for class 'draws_df'
as_draws_df(x, ...)

# S3 method for class 'draws_matrix'
as_draws_df(x, ...)

# S3 method for class 'draws_array'
as_draws_df(x, ...)

# S3 method for class 'draws_list'
as_draws_df(x, ...)

# S3 method for class 'draws_rvars'
as_draws_df(x, ...)

# S3 method for class 'mcmc'
as_draws_df(x, ...)

# S3 method for class 'mcmc.list'
as_draws_df(x, ...)

draws_df(..., .nchains = 1)

is_draws_df(x)
```

## Arguments

- x:

  An object to convert to a `draws_df` object.

- ...:

  For `as_draws_df()`: Arguments passed to individual methods (if
  applicable). For `draws_df()`: Named arguments containing numeric
  vectors each defining a separate variable.

- .nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

A `draws_df` object, which has classes
`c("draws_df", "draws", class(tibble::tibble()))`.

## Details

Objects of class `"draws_df"` are
[tibble](https://tibble.tidyverse.org/reference/tibble.html) data
frames. They have one column per variable as well as additional metadata
columns `".iteration"`, `".chain"`, and `".draw"`. The difference
between the `".iteration"` and `".draw"` columns is that the former is
relative to the MCMC chain while the latter ignores the chain
information and has all unique values. See **Examples**.

If a `data.frame`-like object is supplied to `as_draws_df` that contains
columns named `".iteration"` or `".chain"`, they will be treated as
iteration and chain indices, respectively. See **Examples**.

## See also

Other formats:
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md),
[`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md),
[`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md),
[`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)

## Examples

``` r
x1 <- as_draws_df(example_draws())
class(x1)
#> [1] "draws_df"   "draws"      "tbl_df"     "tbl"        "data.frame"
print(x1)
#> # A draws_df: 100 iterations, 4 chains, and 10 variables
#>      mu tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6]
#> 1  2.01 2.8     3.96    0.271    -0.74      2.1    0.923      1.7
#> 2  1.46 7.0     0.12   -0.069     0.95      7.3   -0.062     11.3
#> 3  5.81 9.7    21.25   14.931     1.83      1.4    0.531      7.2
#> 4  6.85 4.8    14.70    8.586     2.67      4.4    4.758      8.1
#> 5  1.81 2.8     5.96    1.156     3.11      2.0    0.769      4.7
#> 6  3.84 4.1     5.76    9.909    -1.00      5.3    5.889     -1.7
#> 7  5.47 4.0     4.03    4.151    10.15      6.6    3.741     -2.2
#> 8  1.20 1.5    -0.28    1.846     0.47      4.3    1.467      3.3
#> 9  0.15 3.9     1.81    0.661     0.86      4.5   -1.025      1.1
#> 10 7.17 1.8     6.08    8.102     7.68      5.6    7.106      8.5
#> # ... with 390 more draws, and 2 more variables
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
str(x1)
#> draws_df [400 × 13] (S3: draws_df/draws/tbl_df/tbl/data.frame)
#>  $ mu        : num [1:400] 2.01 1.46 5.81 6.85 1.81 ...
#>  $ tau       : num [1:400] 2.77 6.98 9.68 4.79 2.85 ...
#>  $ theta[1]  : num [1:400] 3.962 0.124 21.251 14.7 5.96 ...
#>  $ theta[2]  : num [1:400] 0.271 -0.069 14.931 8.586 1.156 ...
#>  $ theta[3]  : num [1:400] -0.743 0.952 1.829 2.675 3.109 ...
#>  $ theta[4]  : num [1:400] 2.1 7.28 1.38 4.39 1.99 ...
#>  $ theta[5]  : num [1:400] 0.923 -0.062 0.531 4.758 0.769 ...
#>  $ theta[6]  : num [1:400] 1.65 11.26 7.16 8.1 4.66 ...
#>  $ theta[7]  : num [1:400] 3.32 9.62 14.8 9.49 1.21 ...
#>  $ theta[8]  : num [1:400] 4.85 -8.64 -1.74 5.28 -4.54 ...
#>  $ .chain    : int [1:400] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ .iteration: int [1:400] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ .draw     : int [1:400] 1 2 3 4 5 6 7 8 9 10 ...

x2 <- draws_df(a = rnorm(10), b = rnorm(10), c = 1)
class(x2)
#> [1] "draws_df"   "draws"      "tbl_df"     "tbl"        "data.frame"
print(x2)
#> # A draws_df: 10 iterations, 1 chains, and 3 variables
#>         a      b c
#> 1  -0.666 -0.034 1
#> 2  -0.693  0.680 1
#> 3  -0.535  0.010 1
#> 4   0.130  1.754 1
#> 5   0.212 -0.676 1
#> 6   0.119  0.601 1
#> 7   1.522  1.057 1
#> 8   0.006 -0.589 1
#> 9   0.438 -0.520 1
#> 10  0.327  0.705 1
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
str(x2)
#> draws_df [10 × 6] (S3: draws_df/draws/tbl_df/tbl/data.frame)
#>  $ a         : num [1:10] -0.666 -0.693 -0.535 0.13 0.212 ...
#>  $ b         : num [1:10] -0.0341 0.6796 0.0103 1.7538 -0.6757 ...
#>  $ c         : num [1:10] 1 1 1 1 1 1 1 1 1 1
#>  $ .chain    : int [1:10] 1 1 1 1 1 1 1 1 1 1
#>  $ .iteration: int [1:10] 1 2 3 4 5 6 7 8 9 10
#>  $ .draw     : int [1:10] 1 2 3 4 5 6 7 8 9 10

# the difference between iteration and draw is clearer when contrasting
# the head and tail of the data frame
print(head(x1), reserved = TRUE, max_variables = 2)
#> # A draws_df: 6 iterations, 1 chains, and 10 variables
#>    mu tau .chain .iteration .draw
#> 1 2.0 2.8      1          1     1
#> 2 1.5 7.0      1          2     2
#> 3 5.8 9.7      1          3     3
#> 4 6.8 4.8      1          4     4
#> 5 1.8 2.8      1          5     5
#> 6 3.8 4.1      1          6     6
#> # ... with 8 more variables
print(tail(x1), reserved = TRUE, max_variables = 2)
#> # A draws_df: 6 iterations, 1 chains, and 10 variables
#>     mu tau .chain .iteration .draw
#> 1 5.69 2.2      4         95   395
#> 2 3.28 3.3      4         96   396
#> 3 5.04 3.6      4         97   397
#> 4 2.73 6.8      4         98   398
#> 5 0.48 1.8      4         99   399
#> 6 7.05 4.8      4        100   400
#> # ... with 8 more variables

# manually supply chain information
xnew <- data.frame(mu = rnorm(10), .chain = rep(1:2, each = 5))
xnew <- as_draws_df(xnew)
print(xnew)
#> # A draws_df: 5 iterations, 2 chains, and 1 variables
#>       mu
#> 1  -0.10
#> 2  -0.77
#> 3   1.29
#> 4  -1.36
#> 5  -0.34
#> 6   0.24
#> 7  -2.07
#> 8   0.73
#> 9   0.66
#> 10  0.51
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

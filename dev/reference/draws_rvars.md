# The `draws_rvars` format

The `as_draws_rvars()` methods convert objects to the `draws_rvars`
format. The `draws_rvars()` function creates an object of the
`draws_rvars` format based on a set of numeric vectors. See **Details**.

## Usage

``` r
as_draws_rvars(x, ...)

# Default S3 method
as_draws_rvars(x, ...)

# S3 method for class 'draws_rvars'
as_draws_rvars(x, ...)

# S3 method for class 'list'
as_draws_rvars(x, ...)

# S3 method for class 'draws_matrix'
as_draws_rvars(x, ...)

# S3 method for class 'draws_array'
as_draws_rvars(x, ...)

# S3 method for class 'draws_df'
as_draws_rvars(x, ...)

# S3 method for class 'draws_list'
as_draws_rvars(x, ...)

# S3 method for class 'mcmc'
as_draws_rvars(x, ...)

# S3 method for class 'mcmc.list'
as_draws_rvars(x, ...)

draws_rvars(..., .nchains = 1)

is_draws_rvars(x)
```

## Arguments

- x:

  An object to convert to a `draws_rvars` object.

- ...:

  For `as_draws_rvars()`: Arguments passed to individual methods (if
  applicable). For `draws_rvars()`: Named arguments containing numeric
  vectors each defining a separate variable.

- .nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

A `draws_rvars` object, which has classes
`c("draws_rvars", "draws", "list")`.

## Details

Objects of class `"draws_rvars"` are lists of
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) objects.
See **Examples**.

## See also

Other formats:
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md),
[`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md),
[`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
[`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)

## Examples

``` r
x1 <- as_draws_rvars(example_draws())
class(x1)
#> [1] "draws_rvars" "draws"       "list"       
print(x1)
#> # A draws_rvars: 100 iterations, 4 chains, and 3 variables
#> $mu: rvar<100,4>[1] mean ± sd:
#> [1] 4.2 ± 3.4 
#> 
#> $tau: rvar<100,4>[1] mean ± sd:
#> [1] 4.2 ± 3.6 
#> 
#> $theta: rvar<100,4>[8] mean ± sd:
#> [1] 6.7 ± 6.3  5.3 ± 4.6  3.0 ± 6.8  4.9 ± 4.9  3.2 ± 5.1  4.0 ± 5.2  6.5 ± 5.3 
#> [8] 4.6 ± 5.3 
#> 
str(x1)
#> List of 3
#>  $ mu   : rvar<100,4>[1]  4.2 ± 3.4
#>  $ tau  : rvar<100,4>[1]  4.2 ± 3.6
#>  $ theta: rvar<100,4>[8]  6.7 ± 6.3  5.3 ± 4.6  3.0 ± 6.8  4.9 ± 4.9 ...
#>  - attr(*, "class")= chr [1:3] "draws_rvars" "draws" "list"

x2 <- draws_rvars(a = rnorm(10), b = rnorm(10), c = 1)
class(x2)
#> [1] "draws_rvars" "draws"       "list"       
print(x2)
#> # A draws_rvars: 10 iterations, 1 chains, and 3 variables
#> $a: rvar<10>[1] mean ± sd:
#> [1] 0.42 ± 1.2 
#> 
#> $b: rvar<10>[1] mean ± sd:
#> [1] -0.17 ± 0.93 
#> 
#> $c: rvar<10>[1] mean ± sd:
#> [1] 1 ± 0 
#> 
str(x2)
#> List of 3
#>  $ a: rvar<10>[1]  0.42 ± 1.2
#>  $ b: rvar<10>[1]  -0.17 ± 0.93
#>  $ c: rvar<10>[1]  1 ± 0
#>  - attr(*, "class")= chr [1:3] "draws_rvars" "draws" "list"
```

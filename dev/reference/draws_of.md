# Get/set array of draws underlying a random variable

Gets/sets the array-representation that backs an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md). Should be
used rarely.

## Usage

``` r
draws_of(x, with_chains = FALSE)

draws_of(x, with_chains = FALSE) <- value
```

## Arguments

- x:

  (rvar) An
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) object.

- with_chains:

  (logical) Should the array of draws include a dimension for chains? If
  `FALSE` (the default), chains are not included and the array has
  dimension `c(ndraws(x), dim(x))`. If `TRUE`, chains are included and
  the array has dimension `c(niterations(x), nchains(x), dim(x))`.

- value:

  (array) An array of values to use as the backing array of `x`.

## Value

If `with_chains = FALSE`, an array with dimensions
`c(ndraws(x), dim(x))`.

If `with_chains = TRUE`, an array with dimensions
`c(niterations(x), nchains(x), dim(x))`.

## Details

While [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s
implement fast versions of basic math operations (including [matrix
multiplication](https://mc-stan.org/posterior/dev/reference/rvar-matmult.md)),
sometimes you may need to bypass the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
abstraction to do what you need to do more efficiently. `draws_of()`
allows you to get / set the underlying array of draws in order to do
that.

[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s represent
draws internally using arrays of arbitrary dimension, which is returned
by `draws_of(x)` and can be set using `draws_of(x) <- value`. The
**first** dimension of these arrays is the index of the draws. If
`with_chains = TRUE`, then the dimensions of the returned array are
modified so that the first dimension is the index of the iterations and
the second dimension is the index of the chains.

## Examples

``` r
x <- rvar(1:10, nchains = 2)
x
#> rvar<5,2>[1] mean ± sd:
#> [1] 5.5 ± 3 

# draws_of() without arguments will return the array of draws without
# chain information (first dimension is draw)
draws_of(x)
#>    [,1]
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5
#> 6     6
#> 7     7
#> 8     8
#> 9     9
#> 10   10

# draws_of() with with_chains = TRUE will reshape the returned array to
# include chain information in the second dimension
draws_of(x, with_chains = TRUE)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    6
#> [2,]    2    7
#> [3,]    3    8
#> [4,]    4    9
#> [5,]    5   10
#> 

# you can also set draws using draws_of(). When with_chains = FALSE the
# existing chain information will be retained ...
draws_of(x) <- 2:11
x
#> rvar<5,2>[1] mean ± sd:
#> [1] 6.5 ± 3 

# when with_chains = TRUE the chain information will be set by the
# second dimension of the assigned array
draws_of(x, with_chains = TRUE) <- array(2:11, dim = c(2,5))
x
#> rvar<2,5>[1] mean ± sd:
#> [1] 6.5 ± 3 
```

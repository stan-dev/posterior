# Matrix diagonals (including for random variables)

Extract the diagonal of a matrix or construct a matrix, including random
matrices (2-dimensional
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s). Makes
[`base::diag()`](https://rdrr.io/r/base/diag.html) generic.

## Usage

``` r
# S4 method for class 'rvar'
diag(x = 1, nrow, ncol, names = TRUE)
```

## Arguments

- x:

  (numeric,rvar) a matrix, vector, 1D array, missing, or a 1- or
  2-dimensional
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- nrow, ncol:

  optional dimensions for the result when `x` is not a matrix.

- names:

  (when `x` is a matrix) logical indicating if the resulting vector, the
  diagonal of `x`, should inherit
  [`names`](https://rdrr.io/r/base/names.html) from `dimnames(x)` if
  available.

## Value

For [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, has
two modes:

1.  `x` is a matrix-like
    [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md): it
    returns the diagonal as a vector-like
    [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)

2.  `x` is a vector-like
    [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md): it
    returns a matrix-like
    [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with
    `x` as the diagonal and zero for off-diagonal entries.

## Details

Makes [`base::diag()`](https://rdrr.io/r/base/diag.html) into a generic
function. See that function's documentation for usage with
[`numeric`](https://rdrr.io/r/base/numeric.html)s and for usage of
`diag<-`, which is also supported by
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## See also

[`base::diag()`](https://rdrr.io/r/base/diag.html)

## Examples

``` r
# Sigma is a 3x3 covariance matrix
Sigma <- as_draws_rvars(example_draws("multi_normal"))$Sigma
Sigma
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.20  -0.40 ± 0.28 
#> [2,]  0.53 ± 0.20   3.67 ± 0.45  -2.10 ± 0.48 
#> [3,] -0.40 ± 0.28  -2.10 ± 0.48   8.12 ± 0.95 

diag(Sigma)
#> rvar<100,4>[3] mean ± sd:
#> [1] 1.3 ± 0.17  3.7 ± 0.45  8.1 ± 0.95 

diag(Sigma) <- 1:3
Sigma
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.00 ± 0.00   0.53 ± 0.20  -0.40 ± 0.28 
#> [2,]  0.53 ± 0.20   2.00 ± 0.00  -2.10 ± 0.48 
#> [3,] -0.40 ± 0.28  -2.10 ± 0.48   3.00 ± 0.00 

diag(as_rvar(1:3))
#> rvar<1>[3,3] mean ± sd:
#>      [,1]    [,2]    [,3]   
#> [1,] 1 ± NA  0 ± NA  0 ± NA 
#> [2,] 0 ± NA  2 ± NA  0 ± NA 
#> [3,] 0 ± NA  0 ± NA  3 ± NA 
```

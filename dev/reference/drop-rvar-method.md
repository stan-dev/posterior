# Drop redundant dimensions

Delete the dimensions of an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) which are
of size one. See [`base::drop()`](https://rdrr.io/r/base/drop.html)

## Usage

``` r
# S4 method for class 'rvar'
drop(x)
```

## Arguments

- x:

  (rvar) an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with
the same length as `x`, but where any entry equal to `1` in `dim(x)` has
been removed. The exception is if `dim(x) == 1`, in which case
`dim(drop(x)) == 1` as well (this is because
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, unlike
[`numeric`](https://rdrr.io/r/base/numeric.html)s, never have `NULL`
dimensions).

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

Sigma[1, ]
#> rvar<100,4>[1,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.20  -0.40 ± 0.28 

drop(Sigma[1, ])
#> rvar<100,4>[3] mean ± sd:
#> [1]  1.28 ± 0.17   0.53 ± 0.20  -0.40 ± 0.28 

# equivalently ...
Sigma[1, drop = TRUE]
#> rvar<100,4>[1] mean ± sd:
#> [1] 1.3 ± 0.17 
```

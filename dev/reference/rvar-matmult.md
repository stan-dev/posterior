# Matrix multiplication of random variables

Matrix multiplication of random variables.

## Usage

``` r
x %**% y

# S3 method for class 'rvar'
matrixOps(x, y)
```

## Arguments

- x:

  (multiple options) The object to be postmultiplied by `y`:

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)

  - A [`numeric`](https://rdrr.io/r/base/numeric.html) vector or matrix

  - A [`logical`](https://rdrr.io/r/base/logical.html) vector or matrix

  If a vector is used, it is treated as a *row* vector.

- y:

  (multiple options) The object to be premultiplied by `x`:

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)

  - A [`numeric`](https://rdrr.io/r/base/numeric.html) vector or matrix

  - A [`logical`](https://rdrr.io/r/base/logical.html) vector or matrix

  If a vector is used, it is treated as a *column* vector.

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
representing the matrix product of `x` and `y`.

## Details

If `x` or `y` are vectors, they are converted into matrices prior to
multiplication, with `x` converted to a row vector and `y` to a column
vector. Numerics and logicals can be multiplied by
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s and are
broadcasted across all draws of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) argument.
Tensor multiplication is used to efficiently multiply matrices across
draws, so if either `x` or `y` is an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md),
`x %**% y` will be much faster than `rdo(x %*% y)`.

In R \>= 4.3, you can also use `%*%` in place of `%**%` for matrix
multiplication of
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s. In R \<
4.3, S3 classes cannot properly override `%*%`, so you must use `%**%`
for matrix multiplication of
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

## Examples

``` r
# d has mu (mean vector of length 3) and Sigma (3x3 covariance matrix)
d <- as_draws_rvars(example_draws("multi_normal"))
d$Sigma
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.20  -0.40 ± 0.28 
#> [2,]  0.53 ± 0.20   3.67 ± 0.45  -2.10 ± 0.48 
#> [3,] -0.40 ± 0.28  -2.10 ± 0.48   8.12 ± 0.95 

# trivial example: multiplication by a non-random matrix
d$Sigma %**% diag(1:3)
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   1.05 ± 0.40  -1.21 ± 0.85 
#> [2,]  0.53 ± 0.20   7.33 ± 0.89  -6.30 ± 1.44 
#> [3,] -0.40 ± 0.28  -4.20 ± 0.96  24.35 ± 2.84 

# Decompose Sigma into R s.t. R'R = Sigma ...
R <- chol(d$Sigma)
# ... and recreate Sigma using matrix multiplication
t(R) %**% R
#> rvar<100,4>[3,3] mean ± sd:
#>      [,1]          [,2]          [,3]         
#> [1,]  1.28 ± 0.17   0.53 ± 0.20  -0.40 ± 0.28 
#> [2,]  0.53 ± 0.20   3.67 ± 0.45  -2.10 ± 0.48 
#> [3,] -0.40 ± 0.28  -2.10 ± 0.48   8.12 ± 0.95 
```

# Cholesky decomposition of random matrix

Cholesky decomposition of an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) containing
a matrix.

## Usage

``` r
# S3 method for class 'rvar'
chol(x, ...)
```

## Arguments

- x:

  (rvar) A 2-dimensional
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Additional parameters passed on to `chol.tensor()`

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
containing the upper triangular factor of the Cholesky decomposition,
i.e., the matrix \\R\\ such that \\R'R = x\\.

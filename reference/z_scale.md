# Rank normalization

Compute rank normalization for a numeric array. First replace each value
by its rank. Average rank for ties are used to conserve the number of
unique values of discrete quantities. Second, normalize ranks via the
inverse normal transformation.

## Usage

``` r
z_scale(x, c = 3/8)
```

## Arguments

- x:

  (numeric) A scalar, vector, matrix, or array of values.

- c:

  (numeric) Fractional offset used in the back-transformation of ranks.
  Defaults to `3/8`.

## Value

A numeric array of rank normalized values with the same size and
dimension as the input.

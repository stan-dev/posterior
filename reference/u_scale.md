# Rank uniformization

Compute rank uniformization for a numeric array. First replace each
value by its rank. Average rank for ties are used to conserve the number
of unique values of discrete quantities. Second, uniformize ranks using
formula `(r - c) / (S - 2 * c + 1)`, where `r` is a rank, `S` is the
number of values, and `c` is a fractional offset which defaults to c =
3/8 as recommend by Blom (1958).

## Usage

``` r
u_scale(x, c = 3/8)
```

## Arguments

- x:

  (numeric) A scalar, vector, matrix, or array of values.

- c:

  (numeric) Fractional offset used in the back-transformation of ranks.
  Defaults to `3/8`.

## Value

A numeric array of uniformized values with the same size and dimension
as the input.

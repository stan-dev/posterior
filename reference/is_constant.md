# Check if vector is constant

check if a vector is constant, up to a defined tolerance relative to the
magnitude of its values.

## Usage

``` r
is_constant(x, tol = .Machine$double.eps)
```

## Arguments

- x:

  (vector) vector to check if is constant

- tol:

  (numeric) relative tolerance to consider two values equal. `x` is
  constant if `max(x) - min(x)` is at most `tol` times the largest
  absolute value in `x`. Default is `.Machine$double.eps`.

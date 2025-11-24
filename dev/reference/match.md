# Value Matching

Generic version of [`base::match()`](https://rdrr.io/r/base/match.html).
For base vectors, returns a vector of the positions of (first) matches
of its first argument in its second. For
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md)s, returns an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md) of the
matches.

## Usage

``` r
match(x, table, ...)

# Default S3 method
match(x, ...)

# S3 method for class 'rvar'
match(x, ...)

x %in% table
```

## Arguments

- x:

  (multiple options) the values to be matched. Can be:

  - A base vector: see
    [`base::match()`](https://rdrr.io/r/base/match.html)

  - An [rvar](https://mc-stan.org/posterior/dev/reference/rvar.md)

- table:

  (vector) the values to be matched against.

- ...:

  Arguments passed on to
  [`base::match`](https://rdrr.io/r/base/match.html)

  `nomatch`

  :   the value to be returned in the case when no match is found. Note
      that it is coerced to `integer`.

  `incomparables`

  :   a vector of values that cannot be matched. Any value in `x`
      matching a value in this vector is assigned the `nomatch` value.
      For historical reasons, `FALSE` is equivalent to `NULL`.

## Value

When `x` is a base vector, a vector of the same length as `x`.

When `x` is an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md), an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md) the same
shape as `x`.

## Details

For more information on how match behaves with base vectors, see
[`base::match()`](https://rdrr.io/r/base/match.html).

When `x` is an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md), the draws
of `x` are matched against `table` using
[`base::match()`](https://rdrr.io/r/base/match.html), and the result is
returned as an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md).

The implementation of `%in%` here is identical to `base::%in%`, except
it uses the generic version of `match()` so that non-base vectors (such
as [rvar](https://mc-stan.org/posterior/dev/reference/rvar.md)s) are
supported.

## Examples

``` r
x <- rvar(c("a","b","b","c","d"))
x %in% c("b","d")
#> rvar<5>[1] mean ± sd:
#> [1] 0.6 ± 0.55 

# for additional examples, see base::match()
```

# Is `x` a factor random variable?

Test if `x` is an
[`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
or
[`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md).

## Usage

``` r
is_rvar_factor(x)

is_rvar_ordered(x)
```

## Arguments

- x:

  (any object) An object to test.

## Value

`TRUE` if `x` is an
[`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
or
[`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md),
`FALSE` otherwise.

## See also

[`as_rvar_factor()`](https://mc-stan.org/posterior/dev/reference/as_rvar_factor.md)
and
[`as_rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/as_rvar_factor.md)
to convert objects to `rvar_factor`s and `rvar_ordered`s.

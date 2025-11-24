# Random variable resulting from a function applied over margins of an array or random variable

Returns an [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
obtained by applying a function to margins of an array or
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md). Acts like
[`apply()`](https://rdrr.io/r/base/apply.html), except that the function
supplied (`.f`) should return an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), and the
final result is always an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Usage

``` r
rvar_apply(.x, .margin, .f, ...)
```

## Arguments

- .x:

  An array or an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- .margin:

  (multiple options) The subscripts which the function will be applied
  over:

  - An integer vector. E.g., for a matrix `1` indicates rows, `2`
    indicates columns, `c(1, 2)` indicates rows and columns.

  - A character vector of dimension names if `.x` has named dimensions.

- .f:

  (function) The function to be applied. The function `.f` must return
  an [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) and
  the dimensions of the result of `.f` applied to each margin of `.x`
  must be able to be broadcasted to a common shape (otherwise the
  resulting
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) cannot
  be simplified). See **Details**.

- ...:

  Optional arguments passed to `.f`.

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

If the result of each call to `.f` returns an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) of
dimension `d` after being broadcast to a common shape, then
`rvar_apply()` returns an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) of
dimension `c(d, dim(.x)[.margin])`. If the last dimension of the result
would be `1`, it is dropped (other dimensions equal to `1` are
retained). If `d` is `0`, the result has length `0` but not necessarily
the 'correct' dimension.

## Details

This function acts much like
[`apply()`](https://rdrr.io/r/base/apply.html), except that the function
passed to it (`.f`) must return
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, and the
result is simplified into an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md). Unlike
[`apply()`](https://rdrr.io/r/base/apply.html), it also keeps the
dimensions of the returned values along each margin, rather than
simplifying each margin to a vector, and if the results of `.f` do not
all have the same dimensions, it applies the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
broadcasting rules to bind results together rather than using vector
recycling.

If you wish to apply functions over
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s where the
result is not intended to be simplified into an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), you can
use the standard [`apply()`](https://rdrr.io/r/base/apply.html),
[`lapply()`](https://rdrr.io/r/base/lapply.html),
[`sapply()`](https://rdrr.io/r/base/lapply.html), or
[`vapply()`](https://rdrr.io/r/base/lapply.html) functions.

## See also

[`as_rvar()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md) to
convert objects to `rvar`s. See
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), and
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
for higher-level interfaces for creating `rvar`s.

## Examples

``` r
set.seed(3456)
x <- rvar_rng(rnorm, 24, mean = 1:24)
dim(x) <- c(2,3,4)

# we can find the distributions of marginal means of the above array
# using rvar_mean along with rvar_apply
rvar_apply(x, 1, rvar_mean)
#> rvar<4000>[2] mean ± sd:
#> [1] 12 ± 0.29  13 ± 0.29 
rvar_apply(x, 2:3, rvar_mean)
#> rvar<4000>[3,4] mean ± sd:
#>      [,1]         [,2]         [,3]         [,4]        
#> [1,]  1.5 ± 0.70   7.5 ± 0.69  13.5 ± 0.71  19.5 ± 0.70 
#> [2,]  3.5 ± 0.70   9.5 ± 0.71  15.5 ± 0.72  21.5 ± 0.70 
#> [3,]  5.5 ± 0.71  11.5 ± 0.72  17.5 ± 0.71  23.5 ± 0.70 
```

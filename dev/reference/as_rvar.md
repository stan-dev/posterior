# Coerce to a random variable

Convert `x` to an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) object.

## Usage

``` r
as_rvar(x, dim = NULL, dimnames = NULL, nchains = NULL)

as_rvar_numeric(x, dim = NULL, dimnames = NULL, nchains = NULL)

as_rvar_integer(x, dim = NULL, dimnames = NULL, nchains = NULL)

as_rvar_logical(x, dim = NULL, dimnames = NULL, nchains = NULL)
```

## Arguments

- x:

  (multiple options) An object that can be converted to an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), such as
  a vector, array, or an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) itself.

- dim:

  (integer vector) One or more integers giving the maximal indices in
  each dimension to override the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to be
  created (see [`dim()`](https://rdrr.io/r/base/dim.html)). If `NULL`
  (the default), `dim` is determined by the input. **NOTE:** This
  argument controls the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), not the
  underlying array, so you cannot change the number of draws using this
  argument.

- dimnames:

  (list) Character vectors giving the names in each dimension to
  override the names of the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to be
  created (see [`dimnames()`](https://rdrr.io/r/base/dimnames.html)). If
  `NULL` (the default), this is determined by the input. **NOTE:** This
  argument controls the names of the dimensions of the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), not the
  underlying array.

- nchains:

  (positive integer) The number of chains. The default is `1`.

## Value

An object of class `"rvar"` (or one of its subtypes) representing a
random variable.

## Details

For objects that are already
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, returns
them (with modified dimensions if `dim` is not `NULL`).

For numeric or logical vectors or arrays, returns an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with a
single draw and the same dimensions as `x`. This is in contrast to the
[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md)
constructor, which treats the first dimension of `x` as the draws
dimension. As a result, `as_rvar()` is useful for creating constants.

While `as_rvar()` attempts to pick the most suitable subtype of
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) based on
the type of `x` (possibly returning an
[`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
or
[`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)),
`as_rvar_numeric()`, `as_rvar_integer()`, and `as_rvar_logical()` always
coerce the draws of the output
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to be
[`numeric`](https://rdrr.io/r/base/numeric.html),
[`integer`](https://rdrr.io/r/base/integer.html), or
[`logical`](https://rdrr.io/r/base/logical.html) (respectively), and
always return a base
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), never a
subtype.

## See also

[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) to
construct [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s
directly. See
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), and
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
for higher-level interfaces for creating `rvar`s.

## Examples

``` r
# You can use as_rvar() to create "constant" rvars (having only one draw):
x <- as_rvar(1)
x
#> rvar<1>[1] mean ± sd:
#> [1] 1 ± NA 

# Such constants can be of arbitrary shape:
as_rvar(1:4)
#> rvar<1>[4] mean ± sd:
#> [1] 1 ± NA  2 ± NA  3 ± NA  4 ± NA 
as_rvar(matrix(1:10, nrow = 5))
#> rvar<1>[5,2] mean ± sd:
#>      [,1]     [,2]    
#> [1,]  1 ± NA   6 ± NA 
#> [2,]  2 ± NA   7 ± NA 
#> [3,]  3 ± NA   8 ± NA 
#> [4,]  4 ± NA   9 ± NA 
#> [5,]  5 ± NA  10 ± NA 
as_rvar(array(1:12, dim = c(2, 3, 2)))
#> rvar<1>[2,3,2] mean ± sd:
#> , , 1
#> 
#>      [,1]     [,2]     [,3]    
#> [1,]  1 ± NA   3 ± NA   5 ± NA 
#> [2,]  2 ± NA   4 ± NA   6 ± NA 
#> 
#> , , 2
#> 
#>      [,1]     [,2]     [,3]    
#> [1,]  7 ± NA   9 ± NA  11 ± NA 
#> [2,]  8 ± NA  10 ± NA  12 ± NA 
#> 

# as_rvar_numeric() coerces subtypes of rvar to the base rvar type
y <- as_rvar_factor(c("a", "b", "c"))
y
#> rvar_factor<1>[3] mode <entropy>:
#> [1] a <0>  b <0>  c <0> 
#> 3 levels: a b c
as_rvar_numeric(y)
#> rvar<1>[3] mean ± sd:
#> [1] 1 ± NA  2 ± NA  3 ± NA 
```

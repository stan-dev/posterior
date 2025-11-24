# Coerce to a factor random variable

Convert `x` to an
[`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
or
[`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
object.

## Usage

``` r
as_rvar_factor(x, dim = NULL, dimnames = NULL, nchains = NULL, ...)

as_rvar_ordered(x, dim = NULL, dimnames = NULL, nchains = NULL, ...)
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

- ...:

  Arguments passed on to
  [`base::factor`](https://rdrr.io/r/base/factor.html)

  `levels`

  :   an optional vector of the unique values (as character strings)
      that `x` might have taken. The default is the unique set of values
      taken by
      [`as.character`](https://rdrr.io/r/base/character.html)`(x)`,
      sorted into increasing order *of `x`*. Note that this set can be
      specified as smaller than `sort(unique(x))`.

  `labels`

  :   *either* an optional character vector of labels for the levels (in
      the same order as `levels` after removing those in `exclude`),
      *or* a character string of length 1. Duplicated values in `labels`
      can be used to map different values of `x` to the same factor
      level.

  `exclude`

  :   a vector of values to be excluded when forming the set of levels.
      This may be factor with the same level set as `x` or should be a
      `character`.

  `ordered`

  :   logical flag to determine if the levels should be regarded as
      ordered (in the order given).

  `nmax`

  :   an upper bound on the number of levels; see ‘Details’.

## Value

An object of class `"rvar_factor"` or `"rvar_ordered"` representing a
random variable.

## Details

For objects that are already
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s, returns
them (with modified dimensions if `dim` is not `NULL`), possibly adding
levels using the unique values of the draws of the `rvar` (if the object
is not already factor-like).

For numeric, logical, factor, or character vectors or arrays, returns an
[`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
or
[`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
with a single draw and the same dimensions as `x`. This is in contrast
to the
[`rvar_factor()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
and
[`rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
constructors, which treats the first dimension of `x` as the draws
dimension. As a result, `as_rvar_factor()` and `as_rvar_ordered()` are
useful for creating constants.

## See also

[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md),
[`rvar_factor()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md),
and
[`rvar_ordered()`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)
to construct
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s directly.
See [`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), and
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
for higher-level interfaces for creating `rvar`s.

## Examples

``` r
# You can use as_rvar_factor() to create "constant" rvars (having only one draw):
x <- as_rvar_factor("a")
x
#> rvar_factor<1>[1] mode <entropy>:
#> [1] a <0> 
#> 1 levels: a

# Such constants can be of arbitrary shape:
as_rvar_factor(letters[1:4])
#> rvar_factor<1>[4] mode <entropy>:
#> [1] a <0>  b <0>  c <0>  d <0> 
#> 4 levels: a b c d
as_rvar_ordered(matrix(letters[1:10], nrow = 5))
#> rvar_ordered<1>[5,2] mode <dissent>:
#>      [,1]   [,2]  
#> [1,] a <0>  f <0> 
#> [2,] b <0>  g <0> 
#> [3,] c <0>  h <0> 
#> [4,] d <0>  i <0> 
#> [5,] e <0>  j <0> 
#> 10 levels: a < b < c < d < e < f < g < h < i < j
as_rvar_factor(array(letters[1:12], dim = c(2, 3, 2)))
#> rvar_factor<1>[2,3,2] mode <entropy>:
#> , , 1
#> 
#>      [,1]   [,2]   [,3]  
#> [1,] a <0>  c <0>  e <0> 
#> [2,] b <0>  d <0>  f <0> 
#> 
#> , , 2
#> 
#>      [,1]   [,2]   [,3]  
#> [1,] g <0>  i <0>  k <0> 
#> [2,] h <0>  j <0>  l <0> 
#> 
#> 12 levels: a b c d e f g h i j k l
```

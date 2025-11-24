# Factor random variables of arbitrary dimension

Random variables backed by
[factor](https://rdrr.io/r/base/factor.html)-like arrays of arbitrary
dimension.

## Usage

``` r
rvar_factor(
  x = factor(),
  dim = NULL,
  dimnames = NULL,
  nchains = NULL,
  with_chains = FALSE,
  ...
)

rvar_ordered(
  x = ordered(NULL),
  dim = NULL,
  dimnames = NULL,
  nchains = NULL,
  with_chains = FALSE,
  ...
)
```

## Arguments

- x:

  (multiple options) The object to convert to an `rvar`:

  - A vector of draws from a distribution.

  - An array where the first dimension represents draws from a
    distribution. The resulting
    [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) will
    have dimension `dim(x)[-1]`; that is, everything except the first
    dimension is used for the shape of the variable, and the first
    dimension is used to index draws from the distribution (see
    **Examples**). Optionally, if `with_chains == TRUE`, the first
    dimension indexes the iteration and the second dimension indexes the
    chain (see `with_chains`).

  - An `rvar`.

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

  (positive integer) The number of chains. The if `NULL` (the default),
  `1` is used unless `x` is already an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), in
  which case the number of chains it has is used.

- with_chains:

  (logical) Does `x` include a dimension for chains? If `FALSE` (the
  default), chains are not included, the first dimension of the input
  array should index draws, and the `nchains` argument can be used to
  determine the number of chains. If `TRUE`, the `nchains` argument is
  ignored and the second dimension of `x` is used to index chains.
  Internally, the array will be converted to a format without the chain
  index. Ignored when `x` is already an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

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

An object of class `"rvar_factor"` representing a `factor`-like random
variable.

## Details

A subtype of
[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) that
represents a (possibly multidimensional) sample of a
[factor](https://rdrr.io/r/base/factor.html) or an
[ordered](https://rdrr.io/r/base/factor.html) factor. It is otherwise
very similar to the basic
[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md): it is
backed by a multidimensional array with draws as the first dimension.
The primary difference is that the backing array has class `"factor"`
(for `rvar_factor()`) or `c("ordered", "factor")` (for
`rvar_ordered()`). If you pass a
[factor](https://rdrr.io/r/base/factor.html) or
[ordered](https://rdrr.io/r/base/factor.html) factor to
[`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) it will
automatically return an object with the classes `"rvar_factor"` or
`c("rvar_ordered", "rvar_factor")`.

See [`rvar()`](https://mc-stan.org/posterior/dev/reference/rvar.md) for
more details on the internals of the random variable datatype.

## See also

[`as_rvar_factor()`](https://mc-stan.org/posterior/dev/reference/as_rvar_factor.md)
to convert objects to `rvar_factor`s. See
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), and
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
for higher-level interfaces for creating `rvar`s.

## Examples

``` r
set.seed(1234)

# To create a "scalar" `rvar_factor`, pass a one-dimensional array or a vector
# whose length (here `4000`) is the desired number of draws:
x <- rvar(sample(c("a","a","a","b","c"), 4000, replace = TRUE))
x
#> rvar_factor<4000>[1] mode <entropy>:
#> [1] a <0.87> 
#> 3 levels: a b c

# Create random vectors by adding an additional dimension:
x_array <- array(c(
    sample(c("a","a","a","b","c"), 4000, replace = TRUE),
    sample(c("a","a","b","c","c"), 4000, replace = TRUE),
    sample(c("b","b","b","b","c"), 4000, replace = TRUE),
    sample(c("d","d","b","b","c"), 4000, replace = TRUE)
  ), dim = c(4000, 4))
rvar_factor(x_array)
#> rvar_factor<4000>[4] mode <entropy>:
#> [1] a <0.68>  c <0.76>  b <0.36>  b <0.76> 
#> 4 levels: a b c d

# You can also create ordered factors
rvar_ordered(x_array)
#> rvar_ordered<4000>[4] mode <dissent>:
#> [1] a <0.41>  c <0.47>  b <0.17>  b <0.47> 
#> 4 levels: a < b < c < d

# arguments of factor() and ordered() are passed down by the constructor
# e.g. we can reorder levels of an ordered factor:
rvar_ordered(x_array, levels = c("d","c","b","a"))
#> rvar_ordered<4000>[4] mode <dissent>:
#> [1] a <0.41>  c <0.47>  b <0.17>  b <0.47> 
#> 4 levels: d < c < b < a

# Unlike base factors, rvar factors can be matrices or arrays:
rvar_factor(x_array, dim = c(2, 2))
#> rvar_factor<4000>[2,2] mode <entropy>:
#>      [,1]      [,2]     
#> [1,] a <0.68>  b <0.36> 
#> [2,] c <0.76>  b <0.76> 
#> 4 levels: a b c d

# If the input to rvar_factor() is an array with a `"levels"` attribute, it
# will use those as the levels of the factor
y_array <- t(array(rbinom(3000, 1, c(0.1, 0.5, 0.9)) + 1, dim = c(3, 1000)))
rvar(y_array)
#> rvar<1000>[3] mean ± sd:
#> [1] 1.1 ± 0.30  1.5 ± 0.50  1.9 ± 0.29 
# with levels
attr(y_array, "levels") = c("a", "b")
rvar_factor(y_array)
#> rvar_factor<1000>[3] mode <entropy>:
#> [1] a <0.46>  a <1.00>  b <0.45> 
#> 2 levels: a b
```

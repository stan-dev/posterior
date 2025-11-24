# Random variables of arbitrary dimension

Random variables backed by arrays of arbitrary dimension

## Usage

``` r
rvar(
  x = double(),
  dim = NULL,
  dimnames = NULL,
  nchains = NULL,
  with_chains = FALSE
)
```

## Arguments

- x:

  (multiple options) The object to convert to an `rvar`:

  - A vector of draws from a distribution.

  - An array where the first dimension represents draws from a
    distribution. The resulting `rvar` will have dimension `dim(x)[-1]`;
    that is, everything except the first dimension is used for the shape
    of the variable, and the first dimension is used to index draws from
    the distribution (see **Examples**). Optionally, if
    `with_chains == TRUE`, the first dimension indexes the iteration and
    the second dimension indexes the chain (see `with_chains`).

  - An `rvar`.

- dim:

  (integer vector) One or more integers giving the maximal indices in
  each dimension to override the dimensions of the `rvar` to be created
  (see [`dim()`](https://rdrr.io/r/base/dim.html)). If `NULL` (the
  default), `dim` is determined by the input. **NOTE:** This argument
  controls the dimensions of the `rvar`, not the underlying array, so
  you cannot change the number of draws using this argument.

- dimnames:

  (list) Character vectors giving the names in each dimension to
  override the names of the dimensions of the `rvar` to be created (see
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html)). If `NULL` (the
  default), this is determined by the input. **NOTE:** This argument
  controls the names of the dimensions of the `rvar`, not the underlying
  array.

- nchains:

  (positive integer) The number of chains. The if `NULL` (the default),
  `1` is used unless `x` is already an `rvar`, in which case the number
  of chains it has is used.

- with_chains:

  (logical) Does `x` include a dimension for chains? If `FALSE` (the
  default), chains are not included, the first dimension of the input
  array should index draws, and the `nchains` argument can be used to
  determine the number of chains. If `TRUE`, the `nchains` argument is
  ignored and the second dimension of `x` is used to index chains.
  Internally, the array will be converted to a format without the chain
  index. Ignored when `x` is already an `rvar`.

## Value

An object of class `"rvar"` representing a random variable.

## Details

The `"rvar"` class internally represents random variables as arrays of
arbitrary dimension, where the first dimension is used to index draws
from the distribution. Most mathematical operators and functions are
supported, including efficient matrix multiplication and vector and
array-style indexing. The intent is that an `rvar` works as closely as
possible to how a base vector/matrix/array does, with a few differences:

- The default behavior when subsetting is not to drop extra dimensions
  (i.e. the default `drop` argument for `[` is `FALSE`, not `TRUE`).

- Rather than base R-style recycling, `rvar`s use a limited form of
  broadcasting: if an operation is being performed on two vectors with
  different size of the same dimension, the smaller vector will be
  recycled up to the size of the larger one along that dimension so long
  as it has size 1.

For functions that expect base numeric arrays and for which `rvar`s
cannot be used directly as arguments, you can use
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md) or
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) to
translate your code into code that executes across draws from one or
more random variables and returns a random variable as output. Typically
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) offers the
most straightforward translation.

As [`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md) and
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md) incur some
performance cost, you can also operate directly on the underlying array
using the
[`draws_of()`](https://mc-stan.org/posterior/dev/reference/draws_of.md)
function. To re-use existing random number generator functions to
efficiently create `rvar`s, use
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md).

## See also

[`as_rvar()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md) to
convert objects to `rvar`s. See
[`rdo()`](https://mc-stan.org/posterior/dev/reference/rdo.md),
[`rfun()`](https://mc-stan.org/posterior/dev/reference/rfun.md), and
[`rvar_rng()`](https://mc-stan.org/posterior/dev/reference/rvar_rng.md)
for higher-level interfaces for creating `rvar`s.

## Examples

``` r
set.seed(1234)

# To create a "scalar" `rvar`, pass a one-dimensional array or a vector
# whose length (here `4000`) is the desired number of draws:
x <- rvar(rnorm(4000, mean = 1, sd = 1))
x
#> rvar<4000>[1] mean ± sd:
#> [1] 1 ± 1 

# Create random vectors by adding an additional dimension:
n <- 4   # length of output vector
x <- rvar(array(rnorm(4000 * n, mean = rep(1:n, each = 4000), sd = 1), dim = c(4000, n)))
x
#> rvar<4000>[4] mean ± sd:
#> [1] 1 ± 0.99  2 ± 0.99  3 ± 1.00  4 ± 1.02 

# Create a random matrix:
rows <- 4
cols <- 3
x <- rvar(array(rnorm(4000 * rows * cols, mean = 1, sd = 1), dim = c(4000, rows, cols)))
x
#> rvar<4000>[4,3] mean ± sd:
#>      [,1]         [,2]         [,3]        
#> [1,] 1.00 ± 0.98  1.00 ± 1.00  0.97 ± 1.00 
#> [2,] 1.00 ± 1.01  1.01 ± 1.02  0.99 ± 0.99 
#> [3,] 1.02 ± 1.01  0.99 ± 1.00  1.00 ± 0.99 
#> [4,] 1.01 ± 1.01  1.02 ± 1.00  1.00 ± 1.01 

# If the input sample comes from multiple chains, we can indicate that using the
# nchains argument (here, 1000 draws each from 4 chains):
x <- rvar(rnorm(4000, mean = 1, sd = 1), nchains = 4)
x
#> rvar<1000,4>[1] mean ± sd:
#> [1] 0.97 ± 1 

# Or if the input sample has chain information as its second dimension, we can
# use with_chains to create the rvar
x <- rvar(array(rnorm(4000, mean = 1, sd = 1), dim = c(1000, 4)), with_chains = TRUE)
x
#> rvar<1000,4>[1] mean ± sd:
#> [1] 1 ± 1 
```

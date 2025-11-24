# Modal category

Modal category of a vector.

## Usage

``` r
modal_category(x)

# Default S3 method
modal_category(x)

# S3 method for class 'rvar'
modal_category(x)
```

## Arguments

- x:

  (multiple options) A vector to be interpreted as draws from a
  categorical distribution, such as:

  - A [factor](https://rdrr.io/r/base/factor.html)

  - A [numeric](https://rdrr.io/r/base/numeric.html) (should be
    [integer](https://rdrr.io/r/base/integer.html) or integer-like)

  - An [rvar](https://mc-stan.org/posterior/dev/reference/rvar.md),
    [rvar_factor](https://mc-stan.org/posterior/dev/reference/rvar_factor.md),
    or
    [rvar_ordered](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)

## Value

If `x` is a [factor](https://rdrr.io/r/base/factor.html) or
[numeric](https://rdrr.io/r/base/numeric.html), returns a length-1
vector containing the modal value.

If `x` is an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md), returns an
array of the same shape as `x`, where each cell is the modal value of
the draws in the corresponding cell of `x`.

## Details

Finds the modal category (i.e., most frequent value) in `x`. In the case
of ties, returns the first tie.

## Examples

``` r
x <- factor(c("a","b","b","c","d"))
modal_category(x)
#> [1] "b"

# in the case of ties, the first tie is returned
y <- factor(c("a","c","c","d","d"))
modal_category(y)
#> [1] "c"

# both together, as an rvar
xy <- c(rvar(x), rvar(y))
xy
#> rvar_factor<5>[2] mode <entropy>:
#> [1] b <0.96>  c <0.76> 
#> 4 levels: a b c d
modal_category(xy)
#> [1] "b" "c"
```

# Normalized entropy

Normalized entropy, for measuring dispersion in draws from categorical
distributions.

## Usage

``` r
entropy(x)

# Default S3 method
entropy(x)

# S3 method for class 'rvar'
entropy(x)
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
numeric vector with a value between 0 and 1 (inclusive) giving the
normalized Shannon entropy of `x`.

If `x` is an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md), returns an
array of the same shape as `x`, where each cell is the normalized
Shannon entropy of the draws in the corresponding cell of `x`.

## Details

Calculates the normalized Shannon entropy of the draws in `x`. This
value is the entropy of `x` divided by the maximum entropy of a
distribution with `n` categories, where `n` is `length(unique(x))` for
numeric vectors and `length(levels(x))` for factors:

\$\$-\frac{\sum\_{i = 1}^{n} p_i \log(p_i)}{\log(n)}\$\$

This scales the output to be between 0 (all probability in one category)
and 1 (uniform). This form of normalized entropy is referred to as
\\H\_\mathrm{REL}\\ in Wilcox (1967).

## References

Allen R. Wilcox (1967). *Indices of Qualitative Variation* (No.
ORNL-TM-1919). Oak Ridge National Lab., Tenn.

## Examples

``` r
set.seed(1234)

levels <- c("a", "b", "c", "d", "e")

# a uniform distribution: high normalized entropy
x <- factor(
  sample(levels, 4000, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
  levels = levels
)
entropy(x)
#> [1] 0.9999008

# a unimodal distribution: low normalized entropy
y <- factor(
  sample(levels, 4000, replace = TRUE, prob = c(0.95, 0.02, 0.015, 0.01, 0.005)),
  levels = levels
)
entropy(y)
#> [1] 0.1659647

# both together, as an rvar
xy <- c(rvar(x), rvar(y))
xy
#> rvar_factor<4000>[2] mode <entropy>:
#> [1] d <1.00>  a <0.17> 
#> 5 levels: a b c d e
entropy(xy)
#> [1] 0.9999008 0.1659647
```

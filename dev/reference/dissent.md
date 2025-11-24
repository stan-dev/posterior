# Dissention

Dissention, for measuring dispersion in draws from ordinal
distributions.

## Usage

``` r
dissent(x)

# Default S3 method
dissent(x)

# S3 method for class 'rvar'
dissent(x)
```

## Arguments

- x:

  (multiple options) A vector to be interpreted as draws from an ordinal
  distribution, such as:

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
dissention of `x`.

If `x` is an
[rvar](https://mc-stan.org/posterior/dev/reference/rvar.md), returns an
array of the same shape as `x`, where each cell is the dissention of the
draws in the corresponding cell of `x`.

## Details

Calculates Tastle and Wierman's (2007) *dissention* measure:

\$\$-\sum\_{i = 1}^{n} p_i \log_2 \left(1 - \frac{\|x_i -
\mathrm{E}(x)\| }{\max(x) - \min(x)} \right)\$\$

This ranges from 0 (all probability in one category) through 0.5
(uniform) to 1 (bimodal: all probability split equally between the first
and last category).

## References

William J. Tastle, Mark J. Wierman (2007). Consensus and dissention: A
measure of ordinal dispersion. *International Journal of Approximate
Reasoning*. 45(3), 531–545.
[doi:10.1016/j.ijar.2006.06.024](https://doi.org/10.1016/j.ijar.2006.06.024)
.

## Examples

``` r
set.seed(1234)

levels <- c("lowest", "low", "neutral", "high", "highest")

# a bimodal distribution: high dissention
x <- ordered(
  sample(levels, 4000, replace = TRUE, prob = c(0.45, 0.04, 0.02, 0.04, 0.45)),
  levels = levels
)
dissent(x)
#> [1] 0.9303737

# a unimodal distribution: low dissention
y <- ordered(
  sample(levels, 4000, replace = TRUE, prob = c(0.95, 0.02, 0.015, 0.01, 0.005)),
  levels = levels
)
dissent(y)
#> [1] 0.1046236

# both together, as an rvar
xy <- c(rvar(x), rvar(y))
xy
#> rvar_ordered<4000>[2] mode <dissent>:
#> [1] highest <0.93>   lowest <0.10> 
#> 5 levels: lowest < low < neutral < high < highest
dissent(xy)
#> [1] 0.9303737 0.1046236
```

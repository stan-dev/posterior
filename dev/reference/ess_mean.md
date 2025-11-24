# Effective sample size for the mean

Compute an effective sample size estimate for a mean (expectation)
estimate of a single variable.

## Usage

``` r
ess_mean(x, ...)

# Default S3 method
ess_mean(x, ...)

# S3 method for class 'rvar'
ess_mean(x, ...)
```

## Arguments

- x:

  (multiple options) One of:

  - A matrix of draws for a single variable (iterations x chains). See
    [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md).

  - An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

If the input is an array, returns a single numeric value. If any of the
draws is non-finite, that is, `NA`, `NaN`, `Inf`, or `-Inf`, the
returned output will be (numeric) `NA`. Also, if all draws within any of
the chains of a variable are the same (constant), the returned output
will be (numeric) `NA` as well. The reason for the latter is that, for
constant draws, we cannot distinguish between variables that are
supposed to be constant (e.g., a diagonal element of a correlation
matrix is always 1) or variables that just happened to be constant
because of a failure of convergence or other problems in the sampling
process.

If the input is an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), returns
an array of the same dimensions as the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), where
each element is equal to the value that would be returned by passing the
draws array for that element of the
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to this
function.

## References

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). *Bayesian Data Analysis, Third
Edition*. Chapman and Hall/CRC.

## Examples

``` r
mu <- extract_variable_matrix(example_draws(), "mu")
ess_mean(mu)
#> [1] 511.5225

d <- as_draws_rvars(example_draws("multi_normal"))
ess_mean(d$Sigma)
#>          [,1]     [,2]     [,3]
#> [1,] 680.2791 446.2236 481.9080
#> [2,] 446.2236 522.0755 418.0690
#> [3,] 481.9080 418.0690 636.2592
```

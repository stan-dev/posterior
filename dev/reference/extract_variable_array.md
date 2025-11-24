# Extract array of a single (possibly indexed) variable

Extract an array of draws of a single variable, including any dimensions
of variables with indices.

## Usage

``` r
extract_variable_array(x, variable, ...)

# Default S3 method
extract_variable_array(x, variable, ...)

# S3 method for class 'draws'
extract_variable_array(x, variable, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- variable:

  (string) The name of the variable to extract. To extract all
  dimensions from variables with indices (e.g. `"x[1]"`), provide the
  base variable name (e.g. `"x"`).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

An `array` with dimension `niterations(x)` x `nchains(x)` x any
remaining dimensions determined by the indices of the variable `x`.

## See also

Other variable extraction methods:
[`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md),
[`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md)

## Examples

``` r
x <- example_draws(example = "multi_normal")

mu <- extract_variable_array(x, variable = "mu")
str(mu)
#>  num [1:100, 1:4, 1:3] 0.18119 -0.03419 -0.05875 -0.1536 0.00989 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ : NULL

mu1 <- extract_variable_array(x, variable = "mu[1]")
str(mu1)
#>  num [1:100, 1:4, 1] 0.18119 -0.03419 -0.05875 -0.1536 0.00989 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ : NULL

Sigma <- extract_variable_array(x, variable = "Sigma")
str(Sigma)
#>  num [1:100, 1:4, 1:3, 1:3] 1.2 1.14 1.12 1.14 1.19 ...
#>  - attr(*, "dimnames")=List of 4
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ : NULL
#>   ..$ : NULL
```

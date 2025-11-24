# Extract matrix of a single variable

Extract an iterations x chains matrix of draws of a single variable.
This is primarily used for convergence diagnostic functions such as
[`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md).

## Usage

``` r
extract_variable_matrix(x, variable, ...)

# Default S3 method
extract_variable_matrix(x, variable, ...)

# S3 method for class 'draws'
extract_variable_matrix(x, variable, ...)

# S3 method for class 'draws_df'
extract_variable_matrix(x, variable, ...)

# S3 method for class 'draws_list'
extract_variable_matrix(x, variable, ...)

# S3 method for class 'draws_rvars'
extract_variable_matrix(x, variable, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- variable:

  (string) The name of the variable to extract. The name must correspond
  to a scalar variable or must include indices for array variables (e.g.
  `"x[1]"`, `"y[1,2]"`) that result in a scalar variable. To extract all
  dimensions from variables with indices, use
  [`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A `matrix` with dimension iterations x chains.

## See also

Other variable extraction methods:
[`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md),
[`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md)

## Examples

``` r
x <- example_draws()
mu <- extract_variable_matrix(x, variable = "mu")
dim(mu)
#> [1] 100   4
rhat(mu)
#> [1] 1.021923
```

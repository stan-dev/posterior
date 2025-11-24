# Extract draws of a single variable

Extract a vector of draws of a single variable.

## Usage

``` r
extract_variable(x, variable, ...)

# Default S3 method
extract_variable(x, variable, ...)

# S3 method for class 'draws'
extract_variable(x, variable, ...)

# S3 method for class 'draws_df'
extract_variable(x, variable, ...)

# S3 method for class 'draws_list'
extract_variable(x, variable, ...)

# S3 method for class 'draws_rvars'
extract_variable(x, variable, ...)
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

A vector of length equal to the number of draws.

## See also

Other variable extraction methods:
[`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md),
[`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md)

## Examples

``` r
x <- example_draws()
mu <- extract_variable(x, variable = "mu")
str(mu)
#>  num [1:400] 2.01 1.46 5.81 6.85 1.81 ...
```

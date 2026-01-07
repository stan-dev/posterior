# Extract arrays of multiple variables

Extract arrays of draws for multiple variables, returning them as a
named list of arrays. Each array has the same structure as returned by
[`extract_variable_array`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md).

## Usage

``` r
extract_list_of_variable_arrays(x, variables = NULL, ...)

# Default S3 method
extract_list_of_variable_arrays(x, variables = NULL, ...)

# S3 method for class 'draws'
extract_list_of_variable_arrays(x, variables = NULL, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- variables:

  A character vector of variable names to extract, or NULL to extract
  all variables. To extract all dimensions from variables with indices
  (e.g. `"x[1]"`), provide the base variable names (e.g. `"x"`).

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A named list of arrays, where each array has dimension `niterations(x)`
x `nchains(x)` x any remaining dimensions determined by the indices of
the variable.

## See also

Other variable extraction methods:
[`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md),
[`extract_variable_array()`](https://mc-stan.org/posterior/dev/reference/extract_variable_array.md),
[`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md)

## Examples

``` r
x <- example_draws(example = "multi_normal")

# Extract multiple variables at once
vars <- extract_list_of_variable_arrays(x, c("mu", "Sigma"))
str(vars)
#> List of 2
#>  $ mu   : num [1:100, 1:4, 1:3] 0.18119 -0.03419 -0.05875 -0.1536 0.00989 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>  $ Sigma: num [1:100, 1:4, 1:3, 1:3] 1.2 1.14 1.12 1.14 1.19 ...
#>   ..- attr(*, "dimnames")=List of 4
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL

# Extract all variables (uses base variable names)
all_vars <- extract_list_of_variable_arrays(x)
str(all_vars)
#> List of 2
#>  $ mu   : num [1:100, 1:4, 1:3] 0.18119 -0.03419 -0.05875 -0.1536 0.00989 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>  $ Sigma: num [1:100, 1:4, 1:3, 1:3] 1.2 1.14 1.12 1.14 1.19 ...
#>   ..- attr(*, "dimnames")=List of 4
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL

# Extract specific indexed variables
vars2 <- extract_list_of_variable_arrays(x, c("mu[1]", "mu[2]"))
str(vars2)
#> List of 2
#>  $ mu[1]: num [1:100, 1:4, 1] 0.18119 -0.03419 -0.05875 -0.1536 0.00989 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>  $ mu[2]: num [1:100, 1:4, 1] 0.467 0.0926 0.3904 -0.072 -0.2544 ...
#>   ..- attr(*, "dimnames")=List of 3
#>   .. ..$ : NULL
#>   .. ..$ : NULL
#>   .. ..$ : NULL
```

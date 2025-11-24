# Set variable names in `draws` objects

Set variable names for all variables in a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object.
The `set_variables()` form is useful when using pipe operators.

## Usage

``` r
variables(x, ...) <- value

# S3 method for class 'draws_matrix'
variables(x, with_indices = TRUE, ...) <- value

# S3 method for class 'draws_array'
variables(x, with_indices = TRUE, ...) <- value

# S3 method for class 'draws_df'
variables(x, with_indices = TRUE, ...) <- value

# S3 method for class 'draws_list'
variables(x, with_indices = TRUE, ...) <- value

# S3 method for class 'draws_rvars'
variables(x, with_indices = FALSE, ...) <- value

set_variables(x, variables, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

- value, variables:

  (character vector) new variable names.

- with_indices:

  (logical) Should indices be included in variable names? For example,
  if the object includes variables named `"x[1]"` and `"x[2]"`, if
  `TRUE`, `c("x[1]", "x[2]")` is returned; if `FALSE`, only `"x"` is
  returned. Defaults to `TRUE` for all formats except
  [`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md).

## Value

Returns a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object
of the same format as `x`, with variables named as specified.

## Details

`variables(x) <- value` allows you to modify the vector of variable
names, similar to how `names(x) <- value` works for vectors and lists.
For renaming specific variables, `set_variables(x, value)` works
equivalently, but is more intuitive when using the pipe operator.

For renaming specific variables,
[`rename_variables()`](https://mc-stan.org/posterior/dev/reference/rename_variables.md)
may offer a more convenient approach.

## See also

[`variables`](https://mc-stan.org/posterior/dev/reference/variables.md),
[`rename_variables`](https://mc-stan.org/posterior/dev/reference/rename_variables.md),
[`draws-index`](https://mc-stan.org/posterior/dev/reference/draws-index.md)

## Examples

``` r
x <- example_draws()

variables(x)
#>  [1] "mu"       "tau"      "theta[1]" "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"
nvariables(x)
#> [1] 10
variables(x) <- letters[1:nvariables(x)]

# or equivalently...
x <- set_variables(x, letters[1:nvariables(x)])
```

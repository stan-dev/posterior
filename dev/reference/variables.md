# Get variable names from `draws` objects

Get variable names from
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects.

## Usage

``` r
variables(x, ...)

# S3 method for class 'draws_matrix'
variables(x, reserved = FALSE, with_indices = TRUE, ...)

# S3 method for class 'draws_array'
variables(x, reserved = FALSE, with_indices = TRUE, ...)

# S3 method for class 'draws_df'
variables(x, reserved = FALSE, with_indices = TRUE, ...)

# S3 method for class 'draws_list'
variables(x, reserved = FALSE, with_indices = TRUE, ...)

# S3 method for class 'draws_rvars'
variables(x, reserved = FALSE, with_indices = FALSE, ...)

nvariables(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

- reserved:

  (logical) Should reserved variables be included in the output?
  Defaults to `FALSE`. See
  [`reserved_variables`](https://mc-stan.org/posterior/dev/reference/reserved_variables.md)
  for an overview of currently reserved variable names.

- with_indices:

  (logical) Should indices be included in variable names? For example,
  if the object includes variables named `"x[1]"` and `"x[2]"`, if
  `TRUE`, `c("x[1]", "x[2]")` is returned; if `FALSE`, only `"x"` is
  returned. Defaults to `TRUE` for all formats except
  [`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md).

## Value

For `variables()`, a character vector.

For `nvariables()`, a scalar integer.

## Details

`variables()` returns a vector of all variable names, and `nvariables()`
returns the number of variables.

## See also

`variables<-`,
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
```

# Rename variables in `draws` objects

Rename variables in a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object.

## Usage

``` r
rename_variables(.x, ...)

# S3 method for class 'draws'
rename_variables(.x, ...)
```

## Arguments

- .x:

  (draws) A
  [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
  object.

- ...:

  One or more expressions, separated by commas, indicating the variables
  to rename. The variable names can be unquoted (`new_name = old_name`)
  or quoted (`"new_name" = "old_name"`). For non-scalar variables, all
  elements can be renamed together (`"new_name" = "old_name"`) or they
  can be renamed individually (`"new_name[1]" = "old_name[1]"`).

## Value

Returns a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object
of the same format as `.x`, with variables renamed according to the
expressions provided in `...`.

## See also

[`variables`](https://mc-stan.org/posterior/dev/reference/variables.md),
`variables<-`,
[`mutate_variables`](https://mc-stan.org/posterior/dev/reference/mutate_variables.md)

## Examples

``` r
x <- as_draws_df(example_draws())
variables(x)
#>  [1] "mu"       "tau"      "theta[1]" "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"

x <- rename_variables(x, mean = mu, sigma = tau)
variables(x)
#>  [1] "mean"     "sigma"    "theta[1]" "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"

x <- rename_variables(x, b = `theta[1]`) # or b  = "theta[1]"
variables(x)
#>  [1] "mean"     "sigma"    "b"        "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"

# rename all elements of 'theta' at once
x <- rename_variables(x, alpha = theta)
variables(x)
#>  [1] "mean"     "sigma"    "b"        "alpha[2]" "alpha[3]" "alpha[4]"
#>  [7] "alpha[5]" "alpha[6]" "alpha[7]" "alpha[8]"
```

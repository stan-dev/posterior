# Mutate variables in `draws` objects

Mutate variables in a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object.

## Usage

``` r
mutate_variables(.x, ...)

# S3 method for class 'draws_matrix'
mutate_variables(.x, ...)

# S3 method for class 'draws_array'
mutate_variables(.x, ...)

# S3 method for class 'draws_df'
mutate_variables(.x, ...)

# S3 method for class 'draws_list'
mutate_variables(.x, ...)

# S3 method for class 'draws_rvars'
mutate_variables(.x, ...)
```

## Arguments

- .x:

  (draws) A
  [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
  object.

- ...:

  Name-value pairs of expressions, each with either length 1 or the same
  length as in the entire input (i.e., number of iterations or draws).
  The name of each argument will be the name of a new variable, and the
  value will be its corresponding value. Use a `NULL` value in
  `mutate_variables` to drop a variable. New variables overwrite
  existing variables of the same name.

## Value

Returns a
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) object
of the same format as `.x`, with variables mutated according to the
expressions provided in `...`.

## Details

In order to mutate variables in
[`draws_matrix`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
and
[`draws_array`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
objects, they are transformed to
[`draws_df`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
objects first and then transformed back after mutation. As those
transformations are quite expensive for larger number of draws, we
recommend using `mutate_variables` on
[`draws_df`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
and
[`draws_list`](https://mc-stan.org/posterior/dev/reference/draws_list.md)
objects if speed is an issue.

In
[`draws_rvars`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)
objects, the output of each expression in `...` is coerced to an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) object if
it is not already one using
[`as_rvar()`](https://mc-stan.org/posterior/dev/reference/as_rvar.md).

## See also

[`variables`](https://mc-stan.org/posterior/dev/reference/variables.md),
[`rename_variables`](https://mc-stan.org/posterior/dev/reference/rename_variables.md)

## Examples

``` r
x <- as_draws_df(example_draws())
x <- subset(x, variable = c("mu", "tau"))

mutate_variables(x, tau2 = tau^2)
#> # A draws_df: 100 iterations, 4 chains, and 3 variables
#>      mu tau tau2
#> 1  2.01 2.8  7.7
#> 2  1.46 7.0 48.7
#> 3  5.81 9.7 93.6
#> 4  6.85 4.8 22.9
#> 5  1.81 2.8  8.1
#> 6  3.84 4.1 16.7
#> 7  5.47 4.0 15.6
#> 8  1.20 1.5  2.2
#> 9  0.15 3.9 15.4
#> 10 7.17 1.8  3.1
#> # ... with 390 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
mutate_variables(x, scale = 1.96 * tau, lower = mu - scale)
#> # A draws_df: 100 iterations, 4 chains, and 4 variables
#>      mu tau scale lower
#> 1  2.01 2.8   5.4  -3.4
#> 2  1.46 7.0  13.7 -12.2
#> 3  5.81 9.7  19.0 -13.2
#> 4  6.85 4.8   9.4  -2.5
#> 5  1.81 2.8   5.6  -3.8
#> 6  3.84 4.1   8.0  -4.2
#> 7  5.47 4.0   7.7  -2.3
#> 8  1.20 1.5   2.9  -1.7
#> 9  0.15 3.9   7.7  -7.6
#> 10 7.17 1.8   3.5   3.7
#> # ... with 390 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

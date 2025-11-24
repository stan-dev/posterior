# Bind `draws` objects together

Bind multiple
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects
together to form a single `draws` object.

## Usage

``` r
bind_draws(x, ...)

# S3 method for class 'draws_matrix'
bind_draws(x, ..., along = "variable")

# S3 method for class 'draws_array'
bind_draws(x, ..., along = "variable")

# S3 method for class 'draws_df'
bind_draws(x, ..., along = "variable")

# S3 method for class 'draws_list'
bind_draws(x, ..., along = "variable")

# S3 method for class 'draws_rvars'
bind_draws(x, ..., along = "variable")
```

## Arguments

- x:

  (draws) A
  [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
  object. The draws format of `x` will define the format of the returned
  draws object.

- ...:

  (draws) Additional
  [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
  objects to bind to `x`.

- along:

  (string) The dimension along which draws objects should be bound
  together. Possible values are `"variable"` (the default), `"chain"`,
  `"iteration"`, and `"draw"`. Not all options are supported for all
  input formats.

## Value

A `draws` object of the same class as `x`.

## Examples

``` r
x1 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
x2 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
ndraws(x1)
#> [1] 5
ndraws(x2)
#> [1] 5
x3 <- bind_draws(x1, x2, along = "draw")
ndraws(x3)
#> [1] 10

x4 <- draws_matrix(theta = rexp(5))
x5 <- bind_draws(x1, x4, along = "variable")
variables(x5)
#> [1] "alpha" "beta"  "theta"
```

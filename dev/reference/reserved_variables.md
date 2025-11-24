# Reserved variables

Get names of reserved variables from objects in the posterior package.

## Usage

``` r
reserved_variables(x, ...)

# Default S3 method
reserved_variables(x, ...)

# S3 method for class 'draws_matrix'
reserved_variables(x, ...)

# S3 method for class 'draws_array'
reserved_variables(x, ...)

# S3 method for class 'draws_df'
reserved_variables(x, ...)

# S3 method for class 'draws_list'
reserved_variables(x, ...)

# S3 method for class 'draws_rvars'
reserved_variables(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A character vector of reserved variables used in `x`.

## Details

`reserved_variables()` returns the names of reserved variables in use by
an object.

The following variables names are currently reserved for special use
cases in all
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) formats:

- `.log_weight`: Log weights per draw (see
  [`weight_draws`](https://mc-stan.org/posterior/dev/reference/weight_draws.md)).

Further, specific for the
[`draws_df`](https://mc-stan.org/posterior/dev/reference/draws_df.md)
format, there are three additional reserved variables:

- `.chain`: Chain index per draw

- `.iteration`: Iteration index within each chain

- `.draw`: Draw index across chains

More reserved variables may be added in the future.

## Examples

``` r
x <- example_draws()
reserved_variables(x)
#> character(0)

# if we add weights, the `.log_weight` reserved variable is used
x <- weight_draws(x, rexp(ndraws(x)))
reserved_variables(x)
#> [1] ".log_weight"
```

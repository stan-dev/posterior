# Order `draws` objects

Order [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
objects according to iteration and chain number. By default, draws
objects are ordered but subsetting or extracting parts of them may leave
them in an unordered state.

## Usage

``` r
order_draws(x, ...)

# S3 method for class 'draws_matrix'
order_draws(x, ...)

# S3 method for class 'draws_array'
order_draws(x, ...)

# S3 method for class 'draws_df'
order_draws(x, ...)

# S3 method for class 'draws_list'
order_draws(x, ...)

# S3 method for class 'draws_rvars'
order_draws(x, ...)

# S3 method for class 'rvar'
order_draws(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A `draws` object of the same class as `x`.

## See also

[`repair_draws()`](https://mc-stan.org/posterior/dev/reference/repair_draws.md)

## Examples

``` r
x <- as_draws_array(example_draws())
dimnames(x[10:5, 4:3, ])
#> $iteration
#> [1] "10" "9"  "8"  "7"  "6"  "5" 
#> 
#> $chain
#> [1] "4" "3"
#> 
#> $variable
#>  [1] "mu"       "tau"      "theta[1]" "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"
#> 
dimnames(order_draws(x[10:5, 4:3, ]))
#> $iteration
#> [1] "5"  "6"  "7"  "8"  "9"  "10"
#> 
#> $chain
#> [1] "3" "4"
#> 
#> $variable
#>  [1] "mu"       "tau"      "theta[1]" "theta[2]" "theta[3]" "theta[4]"
#>  [7] "theta[5]" "theta[6]" "theta[7]" "theta[8]"
#> 
```

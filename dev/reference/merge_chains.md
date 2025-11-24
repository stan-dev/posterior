# Merge chains of `draws` objects

Merge chains of
[`draws`](https://mc-stan.org/posterior/dev/reference/draws.md) objects
into a single chain. Some operations will trigger an automatic merging
of chains, for example, because chains do not match between two objects
involved in a binary operation. By default, no warning will be issued
when this happens but you can activate one via
`options(posterior.warn_on_merge_chains = TRUE)`.

## Usage

``` r
merge_chains(x, ...)

# S3 method for class 'draws_matrix'
merge_chains(x, ...)

# S3 method for class 'draws_array'
merge_chains(x, ...)

# S3 method for class 'draws_df'
merge_chains(x, ...)

# S3 method for class 'draws_list'
merge_chains(x, ...)

# S3 method for class 'rvar'
merge_chains(x, ...)

# S3 method for class 'draws_rvars'
merge_chains(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

A `draws` object of the same class as `x`.

## Examples

``` r
x <- example_draws()

# draws_array with 4 chains, 100 iters each
str(x)
#>  'draws_array' num [1:100, 1:4, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:100] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...

# draws_array with 1 chain of 400 iterations
str(merge_chains(x))
#>  'draws_array' num [1:400, 1, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:400] "1" "2" "3" "4" ...
#>   ..$ chain    : chr "1"
#>   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...
```

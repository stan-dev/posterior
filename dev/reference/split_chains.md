# Split Chains

Split chains by halving the number of iterations per chain and doubling
the number of chains.

## Usage

``` r
split_chains(x, ...)
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
niterations(x)
#> [1] 100
nchains(x)
#> [1] 4

x <- split_chains(x)
niterations(x)
#> [1] 50
nchains(x)
#> [1] 8
```

# Print summaries of `draws` objects

Print output from
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md).

## Usage

``` r
# S3 method for class 'draws_summary'
print(x, ..., num_args = NULL)
```

## Arguments

- x:

  (draws_summary) A `"draws_summary"` object as output by
  [`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md).

- ...:

  Additional arguments passed to
  [`tibble::print.tbl_df()`](https://tibble.tidyverse.org/reference/formatting.html)

- num_args:

  (named list) Optional arguments passed to
  [num()](https://tibble.tidyverse.org/reference/num.html) for pretty
  printing of summaries. If `NULL` (the default), uses the arguments
  stored in the `"num_args"` attribute of `x`, as set by the `.num_args`
  argument of
  [`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md),
  which itself can be controlled globally via the `posterior.num_args`
  [option](https://rdrr.io/r/base/options.html).

## Value

An invisible version of the input object.

## Examples

``` r
x <- example_draws("eight_schools")

# adjust how summaries are printed when calling summarise_draws()...
summarise_draws(x, .num_args = list(sigfig = 2, notation = "dec"))
#> # A tibble: 10 × 10
#>    variable  mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu         4.2    4.2   3.4   3.6  -0.85   9.4   1.0     558.     322.
#>  2 tau        4.2    3.1   3.6   2.9   0.31  11.    1.0     246.     202.
#>  3 theta[1]   6.7    6.0   6.3   4.9  -1.2   19.    1.0     400.     254.
#>  4 theta[2]   5.3    5.1   4.6   4.3  -2.0   13.    1.0     564.     372.
#>  5 theta[3]   3.0    4.0   6.8   4.9 -10.    12.    1.0     312.     205.
#>  6 theta[4]   4.9    5.0   4.9   4.5  -3.6   12.    1.0     695.     252.
#>  7 theta[5]   3.2    3.7   5.1   4.4  -5.9   11.    1.0     523.     306.
#>  8 theta[6]   4.0    4.1   5.2   4.8  -4.3   12.    1.0     548.     205.
#>  9 theta[7]   6.5    5.9   5.3   4.5  -1.2   15.    1.0     434.     308.
#> 10 theta[8]   4.6    4.6   5.3   4.9  -3.8   12.    1.0     355.     146.

# ... or when printing
s <- summarise_draws(x)
print(s, num_args = list(sigfig = 2, notation = "dec"))
#> # A tibble: 10 × 10
#>    variable  mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu         4.2    4.2   3.4   3.6  -0.85   9.4   1.0     558.     322.
#>  2 tau        4.2    3.1   3.6   2.9   0.31  11.    1.0     246.     202.
#>  3 theta[1]   6.7    6.0   6.3   4.9  -1.2   19.    1.0     400.     254.
#>  4 theta[2]   5.3    5.1   4.6   4.3  -2.0   13.    1.0     564.     372.
#>  5 theta[3]   3.0    4.0   6.8   4.9 -10.    12.    1.0     312.     205.
#>  6 theta[4]   4.9    5.0   4.9   4.5  -3.6   12.    1.0     695.     252.
#>  7 theta[5]   3.2    3.7   5.1   4.4  -5.9   11.    1.0     523.     306.
#>  8 theta[6]   4.0    4.1   5.2   4.8  -4.3   12.    1.0     548.     205.
#>  9 theta[7]   6.5    5.9   5.3   4.5  -1.2   15.    1.0     434.     308.
#> 10 theta[8]   4.6    4.6   5.3   4.9  -3.8   12.    1.0     355.     146.
print(s, num_args = list(digits = 3))
#> # A tibble: 10 × 10
#>    variable  mean median    sd   mad      q5    q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl>  <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu       4.180  4.164 3.402 3.570  -0.854  9.387 1.022  558.017  322.096
#>  2 tau      4.164  3.072 3.576 2.892   0.309 11.038 1.015  246.373  202.023
#>  3 theta[1] 6.749  5.973 6.301 4.875  -1.231 18.874 1.014  400.180  253.919
#>  4 theta[2] 5.253  5.133 4.633 4.250  -1.967 12.536 1.015  564.254  371.803
#>  5 theta[3] 3.044  3.985 6.800 4.938 -10.313 11.877 1.014  312.057  205.244
#>  6 theta[4] 4.858  4.992 4.919 4.510  -3.569 12.200 1.023  694.771  251.894
#>  7 theta[5] 3.223  3.722 5.084 4.377  -5.930 10.822 1.005  522.883  305.761
#>  8 theta[6] 3.987  4.137 5.156 4.808  -4.323 11.535 1.020  548.162  204.756
#>  9 theta[7] 6.503  5.902 5.264 4.545  -1.190 15.375 1.004  434.005  308.006
#> 10 theta[8] 4.565  4.637 5.252 4.889  -3.794 12.243 1.023  355.380  146.273
```

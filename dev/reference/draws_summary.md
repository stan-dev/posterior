# Summaries of `draws` objects

The `summarise_draws()` (and `summarize_draws()`) methods provide a
quick way to get a table of summary statistics and diagnostics. These
methods will convert an object to a `draws` object if it isn't already.
For convenience, a [summary()](https://rdrr.io/r/base/summary.html)
method for `draws` and `rvar` objects are also provided as an alias for
`summarise_draws()` if the input object is a `draws` or `rvar` object.

## Usage

``` r
summarise_draws(.x, ...)

summarize_draws(.x, ...)

# S3 method for class 'draws'
summarise_draws(
  .x,
  ...,
  .args = list(),
  .num_args = getOption("posterior.num_args", list()),
  .cores = 1
)

# S3 method for class 'draws'
summary(object, ...)

# S3 method for class 'rvar'
summarise_draws(.x, ...)

# S3 method for class 'rvar'
summary(object, ...)

default_summary_measures()

default_convergence_measures()

default_mcse_measures()
```

## Arguments

- .x, object:

  (draws) A `draws` object or one coercible to a `draws` object.

- ...:

  Name-value pairs of summary or
  [diagnostic](https://mc-stan.org/posterior/dev/reference/diagnostics.md)
  functions. The provided names will be used as the names of the columns
  in the result *unless* the function returns a named vector, in which
  case the latter names are used. The functions can be specified in any
  format supported by
  [as_function()](https://rlang.r-lib.org/reference/as_function.html).
  See **Examples**.

- .args:

  (named list) Optional arguments passed to the summary functions.

- .num_args:

  (named list) Optional arguments passed to
  [num()](https://tibble.tidyverse.org/reference/num.html) for pretty
  printing of summaries. Can be controlled globally via the
  `posterior.num_args` [option](https://rdrr.io/r/base/options.html).

- .cores:

  (positive integer) The number of cores to use for computing summaries
  for different variables in parallel. Coerced to integer if possible,
  otherwise errors. The default is `.cores = 1`, in which case no
  parallelization is implemented. By default, a socket cluster is used
  on Windows and forks otherwise.

## Value

The `summarise_draws()` methods return a
[tibble](https://tibble.tidyverse.org/reference/tibble.html) data frame.
The first column (`"variable"`) contains the variable names and the
remaining columns contain summary statistics and diagnostics.

The functions `default_summary_measures()`,
`default_convergence_measures()`, and `default_mcse_measures()` return
character vectors of names of the default measures.

## Details

The default summary functions used are the ones specified by
`default_summary_measures()` and `default_convergence_measures()`:

`default_summary_measures()`

- [`mean()`](https://rdrr.io/r/base/mean.html)

- [`median()`](https://rdrr.io/r/stats/median.html)

- [`sd()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)

- [`mad()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)

- [`quantile2()`](https://mc-stan.org/posterior/dev/reference/quantile2.md)

`default_convergence_measures()`

- [`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md)

- [`ess_bulk()`](https://mc-stan.org/posterior/dev/reference/ess_bulk.md)

- [`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md)

The
[`var()`](https://mc-stan.org/posterior/dev/reference/rvar-summaries-over-draws.md)
function should not be used to compute variances due to its inconsistent
behavior with matrices. Instead, please use
[`distributional::variance()`](https://pkg.mitchelloharawild.com/distributional/reference/variance.html).

## See also

[`diagnostics`](https://mc-stan.org/posterior/dev/reference/diagnostics.md)
for a list of available diagnostics and links to their individual help
pages.

## Examples

``` r
x <- example_draws("eight_schools")
class(x)
#> [1] "draws_array" "draws"       "array"      
str(x)
#>  'draws_array' num [1:100, 1:4, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:100] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...

summarise_draws(x)
#> # A tibble: 10 × 10
#>    variable  mean median    sd   mad      q5   q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu        4.18   4.16  3.40  3.57  -0.854  9.39  1.02     558.     322.
#>  2 tau       4.16   3.07  3.58  2.89   0.309 11.0   1.01     246.     202.
#>  3 theta[1]  6.75   5.97  6.30  4.87  -1.23  18.9   1.01     400.     254.
#>  4 theta[2]  5.25   5.13  4.63  4.25  -1.97  12.5   1.02     564.     372.
#>  5 theta[3]  3.04   3.99  6.80  4.94 -10.3   11.9   1.01     312.     205.
#>  6 theta[4]  4.86   4.99  4.92  4.51  -3.57  12.2   1.02     695.     252.
#>  7 theta[5]  3.22   3.72  5.08  4.38  -5.93  10.8   1.01     523.     306.
#>  8 theta[6]  3.99   4.14  5.16  4.81  -4.32  11.5   1.02     548.     205.
#>  9 theta[7]  6.50   5.90  5.26  4.54  -1.19  15.4   1.00     434.     308.
#> 10 theta[8]  4.57   4.64  5.25  4.89  -3.79  12.2   1.02     355.     146.
summarise_draws(x, "mean", "median")
#> # A tibble: 10 × 3
#>    variable  mean median
#>    <chr>    <dbl>  <dbl>
#>  1 mu        4.18   4.16
#>  2 tau       4.16   3.07
#>  3 theta[1]  6.75   5.97
#>  4 theta[2]  5.25   5.13
#>  5 theta[3]  3.04   3.99
#>  6 theta[4]  4.86   4.99
#>  7 theta[5]  3.22   3.72
#>  8 theta[6]  3.99   4.14
#>  9 theta[7]  6.50   5.90
#> 10 theta[8]  4.57   4.64
summarise_draws(x, mean, mcse = mcse_mean)
#> # A tibble: 10 × 3
#>    variable  mean  mcse
#>    <chr>    <dbl> <dbl>
#>  1 mu        4.18 0.150
#>  2 tau       4.16 0.213
#>  3 theta[1]  6.75 0.319
#>  4 theta[2]  5.25 0.202
#>  5 theta[3]  3.04 0.447
#>  6 theta[4]  4.86 0.189
#>  7 theta[5]  3.22 0.232
#>  8 theta[6]  3.99 0.222
#>  9 theta[7]  6.50 0.250
#> 10 theta[8]  4.57 0.273
summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
#> # A tibble: 10 × 3
#>    variable `40%` `60%`
#>    <chr>    <dbl> <dbl>
#>  1 mu        3.41  5.35
#>  2 tau       2.47  3.96
#>  3 theta[1]  4.95  7.01
#>  4 theta[2]  4.32  6.13
#>  5 theta[3]  2.54  5.33
#>  6 theta[4]  3.78  6.11
#>  7 theta[5]  2.69  4.69
#>  8 theta[6]  2.92  5.47
#>  9 theta[7]  4.81  7.33
#> 10 theta[8]  3.50  5.92

# using default_*_meaures()
summarise_draws(x, default_summary_measures())
#> # A tibble: 10 × 7
#>    variable  mean median    sd   mad      q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 mu        4.18   4.16  3.40  3.57  -0.854  9.39
#>  2 tau       4.16   3.07  3.58  2.89   0.309 11.0 
#>  3 theta[1]  6.75   5.97  6.30  4.87  -1.23  18.9 
#>  4 theta[2]  5.25   5.13  4.63  4.25  -1.97  12.5 
#>  5 theta[3]  3.04   3.99  6.80  4.94 -10.3   11.9 
#>  6 theta[4]  4.86   4.99  4.92  4.51  -3.57  12.2 
#>  7 theta[5]  3.22   3.72  5.08  4.38  -5.93  10.8 
#>  8 theta[6]  3.99   4.14  5.16  4.81  -4.32  11.5 
#>  9 theta[7]  6.50   5.90  5.26  4.54  -1.19  15.4 
#> 10 theta[8]  4.57   4.64  5.25  4.89  -3.79  12.2 
summarise_draws(x, default_convergence_measures())
#> # A tibble: 10 × 4
#>    variable  rhat ess_bulk ess_tail
#>    <chr>    <dbl>    <dbl>    <dbl>
#>  1 mu        1.02     558.     322.
#>  2 tau       1.01     246.     202.
#>  3 theta[1]  1.01     400.     254.
#>  4 theta[2]  1.02     564.     372.
#>  5 theta[3]  1.01     312.     205.
#>  6 theta[4]  1.02     695.     252.
#>  7 theta[5]  1.01     523.     306.
#>  8 theta[6]  1.02     548.     205.
#>  9 theta[7]  1.00     434.     308.
#> 10 theta[8]  1.02     355.     146.
summarise_draws(x, default_mcse_measures())
#> # A tibble: 10 × 6
#>    variable mcse_mean mcse_median mcse_sd mcse_q5 mcse_q95
#>    <chr>        <dbl>       <dbl>   <dbl>   <dbl>    <dbl>
#>  1 mu           0.150       0.319   0.249   0.551    0.415
#>  2 tau          0.213       0.250   0.236   0.114    0.964
#>  3 theta[1]     0.319       0.262   0.327   0.820    1.36 
#>  4 theta[2]     0.202       0.213   0.273   0.676    0.848
#>  5 theta[3]     0.447       0.346   0.611   2.18     0.623
#>  6 theta[4]     0.189       0.287   0.251   0.956    0.449
#>  7 theta[5]     0.232       0.139   0.328   1.62     0.736
#>  8 theta[6]     0.222       0.421   0.365   1.16     0.432
#>  9 theta[7]     0.250       0.270   0.260   0.458    0.622
#> 10 theta[8]     0.273       0.372   0.322   0.997    1.29 

# compute variance of variables
summarise_draws(x, var = distributional::variance)
#> # A tibble: 10 × 2
#>    variable   var
#>    <chr>    <dbl>
#>  1 mu        11.6
#>  2 tau       12.8
#>  3 theta[1]  39.7
#>  4 theta[2]  21.5
#>  5 theta[3]  46.2
#>  6 theta[4]  24.2
#>  7 theta[5]  25.9
#>  8 theta[6]  26.6
#>  9 theta[7]  27.7
#> 10 theta[8]  27.6

# illustrate use of '.args'
ws <- rexp(ndraws(x))
summarise_draws(x, weighted.mean, .args = list(w = ws))
#> # A tibble: 10 × 2
#>    variable weighted.mean
#>    <chr>            <dbl>
#>  1 mu                4.37
#>  2 tau               3.93
#>  3 theta[1]          6.56
#>  4 theta[2]          5.17
#>  5 theta[3]          3.01
#>  6 theta[4]          5.00
#>  7 theta[5]          3.42
#>  8 theta[6]          4.08
#>  9 theta[7]          6.35
#> 10 theta[8]          4.82

# adjust how numerical summaries are printed
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
```

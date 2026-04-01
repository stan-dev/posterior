# Resample `draws` objects

Resample [`draws`](https://mc-stan.org/posterior/reference/draws.md)
objects according to provided weights, for example weights obtained
through importance sampling.

## Usage

``` r
resample_draws(x, ...)

# S3 method for class 'draws'
resample_draws(x, weights = NULL, method = "stratified", ndraws = NULL, ...)

# S3 method for class 'rvar'
resample_draws(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

- weights:

  (numeric vector) A vector of positive weights of length `ndraws(x)`.
  The weights will be internally normalized. If `weights` is not
  specified, an attempt will be made to extract any weights already
  stored in the draws object (via
  [`weight_draws()`](https://mc-stan.org/posterior/reference/weight_draws.md)).
  If no weights are stored in the draws object, equal weight is supplied
  to each draw. How exactly the weights influence the resampling depends
  on the `method` argument.

- method:

  (string) The resampling method to use:

  - `"simple"`: simple random resampling with replacement

  - `"simple_no_replace"`: simple random resampling without replacement

  - `"stratified"`: stratified resampling with replacement

  - `"deterministic"`: deterministic resampling with replacement

  Currently, `"stratified"` is the default as it has comparably low
  variance and bias with respect to ideal resampling. The latter would
  sample perfectly proportional to the weights, but this is not possible
  in practice due to the finite number of draws available. For more
  details about resampling methods, see Kitagawa (1996).

- ndraws:

  (positive integer) The number of draws to be returned. By default
  `ndraws` is set internally to the total number of draws in `x` if
  sensible.

## Value

A `draws` object of the same class as `x`.

## Details

Upon usage of `resample_draws()`, chains will automatically be merged
due to subsetting of individual draws (see
[`subset_draws`](https://mc-stan.org/posterior/reference/subset_draws.md)
for details). Also, weights stored in the `draws` object will be removed
in the process, as resampling invalidates existing weights.

## References

Kitagawa, G., Monte Carlo Filter and Smoother for Non-Gaussian Nonlinear
' State Space Models, *Journal of Computational and Graphical
Statistics*, 5(1):1-25, 1996.

## See also

`resample_draws()`

## Examples

``` r
x <- as_draws_df(example_draws())

# random weights for justr for demonstration
w <- runif(ndraws(x), 0, 10)

# use default stratified sampling
x_rs <- resample_draws(x, weights = w)
#> Merging chains in order to subset via 'draw'.
summarise_draws(x_rs, default_summary_measures())
#> # A tibble: 10 × 7
#>    variable  mean median    sd   mad      q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 mu        4.24   4.59  3.45  3.25  -1.17   9.48
#>  2 tau       4.54   3.49  3.81  3.29   0.307 11.7 
#>  3 theta[1]  7.08   6.23  6.51  5.22  -1.16  19.3 
#>  4 theta[2]  5.37   5.13  4.94  4.03  -2.32  14.2 
#>  5 theta[3]  2.96   4.13  7.31  5.22 -11.1   12.0 
#>  6 theta[4]  5.10   5.24  5.01  4.32  -3.63  12.3 
#>  7 theta[5]  2.89   3.57  5.35  4.54  -7.60  10.8 
#>  8 theta[6]  3.88   4.34  5.34  5.00  -6.04  11.1 
#>  9 theta[7]  6.88   6.73  5.29  4.60  -1.15  15.4 
#> 10 theta[8]  4.60   4.73  5.71  5.23  -5.31  14.5 

# use simple random sampling
x_rs <- resample_draws(x, weights = w, method = "simple")
#> Merging chains in order to subset via 'draw'.
summarise_draws(x_rs, default_summary_measures())
#> # A tibble: 10 × 7
#>    variable  mean median    sd   mad      q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 mu        3.95   4.08  3.51  4.00  -1.16   8.74
#>  2 tau       4.25   3.18  3.66  2.66   0.332 11.4 
#>  3 theta[1]  7.04   6.06  6.71  5.72  -1.19  20.1 
#>  4 theta[2]  4.66   4.73  4.68  4.51  -2.60  12.4 
#>  5 theta[3]  2.77   4.16  7.50  5.46 -11.1   11.6 
#>  6 theta[4]  4.99   5.50  4.89  4.49  -2.65  12.2 
#>  7 theta[5]  2.66   3.49  5.34  4.86  -6.86  10.6 
#>  8 theta[6]  3.71   4.07  5.14  4.92  -4.98  10.8 
#>  9 theta[7]  6.45   5.92  5.34  4.60  -1.43  14.8 
#> 10 theta[8]  4.34   4.76  5.56  5.22  -5.29  13.2 
```

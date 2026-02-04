# Resample `draws` objects

Resample [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
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
  [`weight_draws()`](https://mc-stan.org/posterior/dev/reference/weight_draws.md)).
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
[`subset_draws`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
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
#>    variable  mean median    sd   mad     q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
#>  1 mu        4.21   4.18  3.43  3.55 -0.847  9.60
#>  2 tau       4.00   3.01  3.34  2.82  0.386 10.1 
#>  3 theta[1]  6.75   6.10  6.35  5.26 -1.06  18.7 
#>  4 theta[2]  5.17   5.38  4.43  4.14 -2.30  12.3 
#>  5 theta[3]  3.35   3.87  6.46  4.96 -8.27  12.7 
#>  6 theta[4]  5.02   5.25  4.98  4.61 -2.93  12.2 
#>  7 theta[5]  3.27   3.73  4.96  4.44 -4.44  10.8 
#>  8 theta[6]  4.10   4.08  5.05  4.65 -3.83  11.5 
#>  9 theta[7]  6.50   5.66  5.23  4.35 -1.06  15.4 
#> 10 theta[8]  4.47   4.44  4.92  4.80 -3.25  12.1 

# use simple random sampling
x_rs <- resample_draws(x, weights = w, method = "simple")
#> Merging chains in order to subset via 'draw'.
summarise_draws(x_rs, default_summary_measures())
#> # A tibble: 10 × 7
#>    variable  mean median    sd   mad      q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 mu        4.03   3.90  3.37  3.68  -1.24   9.38
#>  2 tau       4.10   3.26  3.43  3.15   0.331 10.6 
#>  3 theta[1]  6.98   6.24  6.82  5.99  -1.93  20.1 
#>  4 theta[2]  4.76   4.87  4.49  3.97  -2.27  12.3 
#>  5 theta[3]  3.08   3.91  6.45  5.15 -10.4   11.5 
#>  6 theta[4]  4.75   4.80  5.08  4.81  -3.45  11.9 
#>  7 theta[5]  3.01   3.57  5.11  4.42  -6.12  11.4 
#>  8 theta[6]  4.14   3.62  5.16  4.66  -4.31  11.7 
#>  9 theta[7]  6.24   5.41  5.20  4.38  -1.36  14.5 
#> 10 theta[8]  4.13   3.97  4.91  4.50  -3.44  11.8 
```

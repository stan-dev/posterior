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
#>    variable  mean median    sd   mad      q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
#>  1 mu        4.24   4.38  3.31  3.42  -0.993  9.16
#>  2 tau       4.28   3.14  3.69  2.92   0.362 11.4 
#>  3 theta[1]  6.80   5.85  6.44  5.22  -1.75  19.4 
#>  4 theta[2]  5.63   5.44  4.77  4.11  -1.56  14.3 
#>  5 theta[3]  2.94   3.84  6.89  4.78 -10.3   11.9 
#>  6 theta[4]  5.00   5.41  5.08  4.57  -4.19  12.2 
#>  7 theta[5]  3.25   3.73  5.06  4.25  -6.08  11.2 
#>  8 theta[6]  3.87   3.97  5.12  4.74  -3.85  11.1 
#>  9 theta[7]  6.67   6.13  5.07  4.35  -1.19  14.8 
#> 10 theta[8]  4.53   4.64  5.20  4.66  -4.36  12.1 

# use simple random sampling
x_rs <- resample_draws(x, weights = w, method = "simple")
#> Merging chains in order to subset via 'draw'.
summarise_draws(x_rs, default_summary_measures())
#> # A tibble: 10 × 7
#>    variable  mean median    sd   mad     q5   q95
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>
#>  1 mu        4.16   3.81  3.46  3.80 -0.993  9.55
#>  2 tau       3.88   2.98  3.34  2.73  0.309  9.70
#>  3 theta[1]  6.40   5.77  6.16  5.28 -1.05  18.7 
#>  4 theta[2]  4.99   5.04  4.48  4.11 -2.61  12.1 
#>  5 theta[3]  3.20   3.84  6.24  4.66 -8.19  11.0 
#>  6 theta[4]  5.05   5.36  4.79  4.81 -2.65  12.2 
#>  7 theta[5]  3.44   3.72  5.24  4.66 -4.45  11.7 
#>  8 theta[6]  4.35   4.82  5.03  4.73 -3.74  11.2 
#>  9 theta[7]  6.27   5.65  5.06  4.18 -1.43  13.8 
#> 10 theta[8]  4.17   4.22  5.08  4.34 -5.26  12.0 
```

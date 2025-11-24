# Example `draws` objects

Objects for use in examples, vignettes, and tests.

## Usage

``` r
example_draws(example = "eight_schools")
```

## Arguments

- example:

  (string) The name of the example `draws` object. See **Details** for
  available options.

## Value

A `draws` object.

## Details

The following example `draws` objects are available.

**eight_schools**: A
[`draws_array`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
object with 100 iterations from each of 4 Markov chains obtained by
fitting the eight schools model described in Gelman et al. (2013) with
[Stan](https://mc-stan.org). The variables are:

- `mu`: Overall mean of the eight schools

- `tau`: Standard deviation between schools

- `theta`: Individual means of each of the eight schools

**multi_normal**: A
[`draws_array`](https://mc-stan.org/posterior/dev/reference/draws_array.md)
object with 100 iterations from each of the 4 Markov chains obtained by
fitting a 3-dimensional multivariate normal model to 100 simulated
observations. The variables are:

- `mu`: Mean parameter vector of length 3

- `Sigma`: Covariance matrix of dimension 3 x 3

## Note

These objects are only intended to be used in demonstrations and tests.
They contain fewer iterations and chains than recommended for performing
actual inference.

## References

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). Bayesian Data Analysis, Third
Edition. Chapman and Hall/CRC.

## Examples

``` r
draws_eight_schools <- example_draws("eight_schools")
summarise_draws(draws_eight_schools)
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

draws_multi_normal <- example_draws("multi_normal")
summarise_draws(draws_multi_normal)
#> # A tibble: 12 × 10
#>    variable      mean  median    sd   mad     q5     q95  rhat ess_bulk ess_tail
#>    <chr>        <dbl>   <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu[1]       0.0514  0.0575 0.112 0.131 -0.130  0.225  1.01      677.     356.
#>  2 mu[2]       0.111   0.104  0.199 0.198 -0.208  0.449  1.00      566.     426.
#>  3 mu[3]       0.186   0.184  0.314 0.315 -0.322  0.715  1.02      650.     334.
#>  4 Sigma[1,1]  1.28    1.26   0.165 0.173  1.03   1.56   1.00      742.     369.
#>  5 Sigma[2,1]  0.525   0.502  0.200 0.173  0.227  0.874  1.01      454.     239.
#>  6 Sigma[3,1] -0.403  -0.393  0.282 0.267 -0.874  0.0432 1.01      468.     308.
#>  7 Sigma[1,2]  0.525   0.502  0.200 0.173  0.227  0.874  1.01      454.     239.
#>  8 Sigma[2,2]  3.67    3.62   0.447 0.433  3.02   4.40   1.01      529.     363.
#>  9 Sigma[3,2] -2.10   -2.11   0.480 0.469 -2.87  -1.39   1.02      434.     357.
#> 10 Sigma[1,3] -0.403  -0.393  0.282 0.267 -0.874  0.0432 1.01      468.     308.
#> 11 Sigma[2,3] -2.10   -2.11   0.480 0.469 -2.87  -1.39   1.02      434.     357.
#> 12 Sigma[3,3]  8.12    8.02   0.946 0.941  6.71   9.91   0.997     729.     324.
```

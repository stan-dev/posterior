
<!-- README.md is generated from README.Rmd. Please edit that file -->

# posterior <img src="man/figures/stanlogo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/posterior)](https://CRAN.R-project.org/package=posterior)
[![Travis Build
Status](https://travis-ci.org/stan-dev/posterior.svg?branch=master)](https://travis-ci.org/stan-dev/posterior)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/stan-dev/posterior?branch=master&svg=true)](https://ci.appveyor.com/project/jgabry/posterior)
[![Coverage
Status](https://codecov.io/gh/stan-dev/posterior/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/posterior)
<!-- badges: end -->

The **posterior** R package is intended to provide useful tools for both
users and developers of packages for fitting Bayesian models or working
with output from Bayesian models. The primary goals of the package are
to:

  - Efficiently convert between many different useful formats of draws
    (samples) from posterior or prior distributions.
  - Provide various summaries of draws in convenient formats.
  - Provide lightweight implementations of state of the art MCMC
    diagnostics.

### Installation

The package is not released yet. Currently you can install the
development version from GitHub, but expect frequent changes until an
official release.

``` r
# install.packages("remotes")
remotes::install_github("stan-dev/posterior")
```

### Examples

``` r
library("posterior")
#> This is posterior version 0.0.4
```

To demonstrate how to work with the **posterior** package, we will use
example posterior draws obtained from the eight schools hierarchical
meta-analysis model described in Gelman et al. (2013). Essentially, we
have an estimate per school (`theta[1]` through `theta[8]`) as well as
an overall mean (`mu`) and standard deviation across schools (`tau`).

#### Draws formats

``` r
eight_schools_array <- example_draws("eight_schools")
print(eight_schools_array, max_variables = 3)
#> # A draws_array: 100 iterations, 4 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration   1    2     3   4
#>         1 2.0  3.0  1.79 6.5
#>         2 1.5  8.2  5.99 9.1
#>         3 5.8 -1.2  2.56 0.2
#>         4 6.8 10.9  2.79 3.7
#>         5 1.8  9.8 -0.03 5.5
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration   1    2    3   4
#>         1 2.8 2.80  8.7 3.8
#>         2 7.0 2.76  2.9 6.8
#>         3 9.7 0.57  8.4 5.3
#>         4 4.8 2.45  4.4 1.6
#>         5 2.8 2.80 11.0 3.0
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration     1     2    3     4
#>         1  3.96  6.26 13.3  5.78
#>         2  0.12  9.32  6.3  2.09
#>         3 21.25 -0.97 10.6 15.72
#>         4 14.70 12.45  5.4  2.69
#>         5  5.96  9.75  8.2 -0.91
#> 
#> # ... with 95 more iterations, and 7 more variables
```

The draws for this example come as a `draws_array` object, that is, an
array with dimensions iterations x chains x variables. We can easily
transform it to another format, for instance, a data frame with
additional meta information.

``` r
eight_schools_df <- as_draws_df(eight_schools_array)
print(eight_schools_df)
#> # A draws_df: 100 iterations, 4 chains, and 10 variables
#>      mu tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6]
#> 1  2.01 2.8     3.96    0.271    -0.74      2.1    0.923      1.7
#> 2  1.46 7.0     0.12   -0.069     0.95      7.3   -0.062     11.3
#> 3  5.81 9.7    21.25   14.931     1.83      1.4    0.531      7.2
#> 4  6.85 4.8    14.70    8.586     2.67      4.4    4.758      8.1
#> 5  1.81 2.8     5.96    1.156     3.11      2.0    0.769      4.7
#> 6  3.84 4.1     5.76    9.909    -1.00      5.3    5.889     -1.7
#> 7  5.47 4.0     4.03    4.151    10.15      6.6    3.741     -2.2
#> 8  1.20 1.5    -0.28    1.846     0.47      4.3    1.467      3.3
#> 9  0.15 3.9     1.81    0.661     0.86      4.5   -1.025      1.1
#> 10 7.17 1.8     6.08    8.102     7.68      5.6    7.106      8.5
#> # ... with 390 more draws, and 2 more variables
#> # ... hidden meta-columns {'.chain', '.iteration', '.draw'}
```

Different formats are preferable in different situations and hence
posterior supports multiple formats and easy conversion between them.
For more details on the available formats see `help("draws")`. All of
the formats are essentially base R object classes and can be used as
such. For example, a `draws_matrix` object is just a `matrix` with a
little more consistency and additional methods.

#### Draws summaries

Computing summaries of posterior or prior draws and convergence
diagnostics for posterior draws is one of the most common tasks when
working with Bayesian models fit using Markov Chain Monte Carlo (MCMC)
methods. The **posterior** package provides a flexible interface for
this purpose via `summarise_draws()`:

``` r
# summarise_draws or summarize_draws
summarise_draws(eight_schools_df)
#> # A tibble: 10 x 10
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
```

Basically, we get a data frame with one row per variable and one column
per summary statistic or convergence diagnostic. We can choose which
summaries to compute by passing additional arguments, either functions
or names of functions. For instance, if we only wanted the mean and its
corresponding Monte Carlo Standard Error (MCSE) we would use:

``` r
summarise_draws(eight_schools_df, "mean", "mcse_mean")
#> # A tibble: 10 x 3
#>    variable  mean mcse_mean
#>    <chr>    <dbl>     <dbl>
#>  1 mu        4.18     0.150
#>  2 tau       4.16     0.213
#>  3 theta[1]  6.75     0.319
#>  4 theta[2]  5.25     0.202
#>  5 theta[3]  3.04     0.447
#>  6 theta[4]  4.86     0.189
#>  7 theta[5]  3.22     0.232
#>  8 theta[6]  3.99     0.222
#>  9 theta[7]  6.50     0.250
#> 10 theta[8]  4.57     0.273
```

For a function to work with `summarise_draws`, it needs to take a vector
or matrix of numeric values and returns a single numeric value or a
named vector of numeric values.

#### Subsetting draws

Another common task when working with posterior (or prior) draws, is
subsetting according to various aspects of the draws (iterations,
chains, or variables). **posterior** provides a convienent interface for
this purpose via the `subset()` method. For example, here is the code to
extract the first five iterations of the first two chains of the
variable `mu`:

``` r
subset_draws(eight_schools_df, variable = "mu", chain = 1:2, iteration = 1:5)
#> # A draws_df: 5 iterations, 2 chains, and 1 variables
#>      mu
#> 1   2.0
#> 2   1.5
#> 3   5.8
#> 4   6.8
#> 5   1.8
#> 6   3.0
#> 7   8.2
#> 8  -1.2
#> 9  10.9
#> 10  9.8
#> # ... hidden meta-columns {'.chain', '.iteration', '.draw'}
```

The same call to `subset_draws()` can be used regardless of whether the
object is a `draws_df`, `draws_array`, `draws_list`, etc.

#### Converting from regular R objects to draws formats

The `eight_schools` example already comes in a format natively supported
by posterior but we could of course also import the draws from other
sources, for example, from common base R objects:

``` r
x <- matrix(rnorm(50), nrow = 10, ncol = 5)
colnames(x) <- paste0("V", 1:5)
x <- as_draws_matrix(x)
print(x)
#> # A draws_matrix: 10 draws, and 5 variables
#>     variable
#> draw    V1    V2      V3     V4    V5
#>   1  -1.70 -0.48 -0.5510 -0.939 -0.37
#>   2  -0.54 -0.54 -0.4694 -0.063  1.93
#>   3  -0.48  1.02 -0.8398  0.332  0.39
#>   4  -0.15  1.17  0.0177  2.015  0.30
#>   5  -1.58  0.24  0.3077 -0.290  1.93
#>   6  -1.09 -1.23  1.0036  0.796 -1.16
#>   7   0.92  1.79  0.4623  0.217  0.98
#>   8  -2.51 -0.41 -1.3883  0.242  0.38
#>   9  -0.35  0.17 -2.2000  0.735 -0.26
#>   10 -1.13  0.79  0.0045  0.655 -2.34

summarise_draws(x, "mean", "sd", "median", "mad")
#> # A tibble: 5 x 5
#>   variable   mean    sd median   mad
#>   <chr>     <dbl> <dbl>  <dbl> <dbl>
#> 1 V1       -0.861 0.958 -0.817 0.841
#> 2 V2        0.252 0.937  0.207 1.07 
#> 3 V3       -0.365 0.942 -0.232 0.851
#> 4 V4        0.370 0.780  0.287 0.604
#> 5 V5        0.178 1.31   0.337 1.00
```

Instead of `as_draws_matrix()` we also could have just used
`as_draws()`, which attempts to find the closest available format to the
input object. In this case this would result in a `draws_matrix` object
either way.

### Contributing

We welcome contributions\! The **posterior** package is under active
development. If you find bugs or have ideas for new features (for us or
yourself to implement) please open an issue on GitHub
(<https://github.com/stan-dev/posterior/issues>).

### References

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). Bayesian Data Analysis, Third
Edition. Chapman and Hall/CRC.

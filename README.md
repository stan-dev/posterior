<!-- README.md is generated from README.Rmd. Please edit that file -->

posterior
=========

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/jgabry/posterior.svg?branch=master)](https://travis-ci.org/jgabry/posterior)
[![Coverage
Status](https://codecov.io/github/jgabry/posterior/coverage.svg?branch=master)](https://codecov.io/github/jgabry/posterior?branch=master)
<!-- badges: end -->

Overview
--------

The **posterior** R package is intended to provide useful tools for both
users and developers of packages for fitting Bayesian models or working
with output from Bayesian models. The primary goals of the package are
to:

-   Efficiently convert between many different useful formats of draws
    (samples) from posterior or prior distributions.
-   Provide various summaries of draws in convenient formats.
-   Provide lightweight implementations of state of the art MCMC
    diagnostics.

How to use posterior
--------------------

``` r
library(posterior)
```

To demonstrate how to work with the posterior package, we will apply it
to posterior draws obtained from the eight schools model described in
Gelman et al. (2013). Essentially, we have an estimate per school
(variables `theta[1]` to `theta[8]`) as well as an overall mean and
standard deviation across schools (variables `mu` and `tau`,
respectively).

``` r
eight_schools <- example_draws("eight_schools")
```

The draws for this example come as a `draws_array` object, that is, an
array with dimensions iterations x chains x variables. We can easily
transform it to another format, for instance, data frame with additional
meta information.

``` r
eight_schools <- as_draws_df(eight_schools)
print(eight_schools)
#> # A tibble: 400 x 13
#>    .chain .iteration .draw    mu    tau `theta[1]` `theta[2]` `theta[3]` `theta[4]`
#>     <int>      <int> <dbl> <dbl>  <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
#>  1      1          1     1 -4.78  1.62      -1.61       -4.96      -5.41    -2.88  
#>  2      1          2     2  6.92  3.40      11.0         9.34       8.60     8.37  
#>  3      1          3     3 11.6   2.37      15.8        11.0       14.9     10.7   
#>  4      1          4     4  3.37 14.6       15.4         6.84      -3.31     5.56  
#>  5      1          5     5  1.33  4.78       3.30        3.09       5.27     2.10  
#>  6      1          6     6 -1.23  6.31       0.452       1.05      -9.22    -4.71  
#>  7      1          7     7  8.35  0.248      8.40        8.09       8.67     8.53  
#>  8      1          8     8  1.63  4.42       8.41        4.09      -6.82    -0.0910
#>  9      1          9     9  8.51  1.13       7.97        8.70      12.1      9.24  
#> 10      1         10    10  6.41  1.35       5.72        4.34       4.39     6.46  
#> # ... with 390 more rows, and 4 more variables: `theta[5]` <dbl>, `theta[6]` <dbl>,
#> #   `theta[7]` <dbl>, `theta[8]` <dbl>
```

Different formats are preferable in different situations and hence
posterior supports multiple formats and easy conversion between them.
For more details see `help("draws")`.

Summarising posterior draws and computing convergnece diagnostics is one
of the most common tasks when working with Markov Chain Monte Carlo
(MCMC) draws. The posterior package provides a flexible interface for
thus purpose with sensible defaults.

``` r
summarise_draws(eight_schools)
#> # A tibble: 10 x 10
#>    variable  mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu        4.56   4.49  3.36  3.45 -0.889  9.88 1.00      878.     300.
#>  2 tau       3.85   2.90  3.32  2.65  0.331  9.98 0.998     387.     311.
#>  3 theta[1]  6.57   5.47  6.45  4.92 -1.50  17.8  1.000     551.     272.
#>  4 theta[2]  4.74   4.53  4.63  4.14 -2.42  12.2  1.04      765.     344.
#>  5 theta[3]  4.22   4.52  5.03  4.63 -4.32  12.2  1.02      553.     246.
#>  6 theta[4]  4.79   4.95  4.45  4.65 -2.24  12.0  0.998     655.     370.
#>  7 theta[5]  3.75   3.85  4.89  4.25 -4.80  11.1  1.000     608.     326.
#>  8 theta[6]  4.28   4.36  4.88  4.65 -4.21  12.3  0.998     643.     305.
#>  9 theta[7]  6.53   6.18  5.38  4.52 -0.763 15.9  0.995     622.     345.
#> 10 theta[8]  5.00   4.52  5.21  4.55 -2.83  13.3  1.01      618.     332.
```

Basically, we get a data frame one row per variable and one column
summary statistics or convergence diagnostics. We can choose which
summaries to compute via the `measures` argument. For instance, if we
were just interested in the mean and corresponding Monto Carlo Standard
Error (MCSE), we could write

``` r
summarise_draws(eight_schools, measures = c("mean", "mcse_mean"))
#> # A tibble: 10 x 3
#>    variable  mean mcse_mean
#>    <chr>    <dbl>     <dbl>
#>  1 mu        4.56     0.113
#>  2 tau       3.85     0.165
#>  3 theta[1]  6.57     0.300
#>  4 theta[2]  4.74     0.176
#>  5 theta[3]  4.22     0.220
#>  6 theta[4]  4.79     0.175
#>  7 theta[5]  3.75     0.198
#>  8 theta[6]  4.28     0.198
#>  9 theta[7]  6.53     0.232
#> 10 theta[8]  5.00     0.215
```

For a measure to work, there needs to be a function defined with the
same name as specified in `measures` which takes a vector or matrix of
numeric values and returns a single numeric value or a named vector of
numeric values.

Another common task when working with posterior (or prior) draws, is
subsetting various aspects of the draws, that is, iterations, chains, or
variables. posterior provides a convienent interface for this purpose
via the `subset` method. For instance, if we want to extract the first 5
iterations of the first two chains of the variables `mu`, we can write

``` r
mu <- subset(eight_schools, variable = "mu", chain = 1:2, iteration = 1:5)
mu
#> # A tibble: 10 x 4
#>    .chain .iteration .draw    mu
#>     <int>      <int> <dbl> <dbl>
#>  1      1          1     1 -4.78
#>  2      1          2     2  6.92
#>  3      1          3     3 11.6 
#>  4      1          4     4  3.37
#>  5      1          5     5  1.33
#>  6      2          1     6 10.9 
#>  7      2          2     7  4.08
#>  8      2          3     8  5.67
#>  9      2          4     9  2.27
#> 10      2          5    10  3.54
```

The `eight_schools` example already comes in a format natively supported
by posterior but we could of course also import the draws from other
sources, for example, from common base R objects.

``` r
x <- matrix(rnorm(50), nrow = 10, ncol = 5)
colnames(x) <- paste0("V", 1:5)
x <- as_draws_matrix(x)
head(x)
#>     variable
#> draw          V1          V2         V3          V4         V5
#>    1 -0.46572509 -0.00488813  0.6061416  0.88225982 -0.5952354
#>    2 -0.79117217 -0.40600382 -0.5127453  0.42769260 -0.1232202
#>    3 -0.15215536 -0.26181763 -1.4353462 -0.03474504 -1.8125592
#>    4 -0.21933591 -0.24057212  1.1645277 -0.19201152 -0.3296461
#>    5  0.09434903 -1.44727585 -2.4372994  0.69270231  1.3265395
#>    6 -0.02862217  1.74869716 -0.9398305 -0.34068929  0.8509511
#> attr(,"class")
#> [1] "draws_matrix" "draws"        "matrix"
```

The posterior is under active development. If you find bugs or have
ideas for new features, you are very welcome to open an issue on GitHub
(<a href="https://github.com/jgabry/posterior" class="uri">https://github.com/jgabry/posterior</a>).

Installation
------------

The package is not released yet. Currently you can install the
development version from GitHub, but expect frequent changes until an
official release.

``` r
# install.packages("remotes")
remotes::install_github("jgabry/posterior")
```

References
----------

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). Bayesian Data Analysis, Third
Edition. Chapman and Hall/CRC.

Aki Vehtari, Andrew Gelman, Daniel Simpson, Bob Carpenter, and
Paul-Christian Bürkner (2019). Rank-normalization, folding, and
localization: An improved R-hat for assessing convergence of MCMC. arXiv
preprint arXiv:1903.08008.

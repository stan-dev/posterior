
<!-- README.md is generated from README.Rmd. Please edit that file -->

# posterior

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/posterior)](https://CRAN.R-project.org/package=posterior)
[![Travis Build
Status](https://travis-ci.org/jgabry/posterior.svg?branch=master)](https://travis-ci.org/jgabry/posterior)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jgabry/posterior?branch=master&svg=true)](https://ci.appveyor.com/project/jgabry/posterior)
[![Coverage
Status](https://codecov.io/gh/jgabry/posterior/branch/master/graph/badge.svg)](https://codecov.io/gh/jgabry/posterior)
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
remotes::install_github("jgabry/posterior")
```

### Examples

``` r
library("posterior")
#> This is posterior version 0.0.0.9000
```

To demonstrate how to work with the **posterior** package, we will use
example posterior draws obtained from the eight schools hierarchical
meta-analysis model described in Gelman et al. (2013). Essentially, we
have an estimate per school (`theta[1]` through `theta[8]`) as well as
an overall mean (`mu`) and standard deviation across schools (`tau`).

#### Draws formats

``` r
eight_schools_array <- example_draws("eight_schools")
str(eight_schools_array)
#>  'draws_array' num [1:100, 1:4, 1:10] -4.78 6.92 11.65 3.37 1.33 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ iteration: chr [1:100] "1" "2" "3" "4" ...
#>   ..$ chain    : chr [1:4] "1" "2" "3" "4"
#>   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...
```

The draws for this example come as a `draws_array` object, that is, an
array with dimensions iterations x chains x variables. We can easily
transform it to another format, for instance, a data frame with
additional meta information.

``` r
eight_schools_df <- as_draws_df(eight_schools_array)
str(eight_schools_df)
#> Classes 'draws_df', 'draws', 'tbl_df', 'tbl' and 'data.frame':   400 obs. of  13 variables:
#>  $ .chain    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ .iteration: int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ .draw     : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ mu        : num  -4.78 6.92 11.65 3.37 1.33 ...
#>  $ tau       : num  1.62 3.4 2.37 14.62 4.78 ...
#>  $ theta[1]  : num  -1.61 10.97 15.77 15.42 3.3 ...
#>  $ theta[2]  : num  -4.96 9.34 11.04 6.84 3.09 ...
#>  $ theta[3]  : num  -5.41 8.6 14.87 -3.31 5.27 ...
#>  $ theta[4]  : num  -2.88 8.37 10.74 5.56 2.1 ...
#>  $ theta[5]  : num  -3.411 10.541 8.736 3.827 0.327 ...
#>  $ theta[6]  : num  -5.2 5.4 8.87 7.94 -1.33 ...
#>  $ theta[7]  : num  -4.15 7.82 9.48 23.2 1.83 ...
#>  $ theta[8]  : num  -5.11 6.33 10.14 6.12 4.01 ...

print(eight_schools_df)
#> # A tibble: 400 x 13
#>    .chain .iteration .draw    mu    tau `theta[1]` `theta[2]` `theta[3]` `theta[4]`
#>     <int>      <int> <int> <dbl>  <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
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
#> # … with 390 more rows, and 4 more variables: `theta[5]` <dbl>, `theta[6]` <dbl>,
#> #   `theta[7]` <dbl>, `theta[8]` <dbl>
```

Different formats are preferable in different situations and hence
posterior supports multiple formats and easy conversion between them.
For more details on the available formats see `help("draws")`.

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
#>    variable  mean median    sd   mad     q5   q95  rhat ess_bulk ess_tail
#>    <chr>    <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
#>  1 mu        4.56   4.49  3.36  3.45 -0.889  9.88 1.00      881.     300.
#>  2 tau       3.85   2.90  3.32  2.65  0.331  9.98 0.998     386.     311.
#>  3 theta[1]  6.57   5.47  6.45  4.92 -1.50  17.8  1.000     552.     272.
#>  4 theta[2]  4.74   4.53  4.63  4.14 -2.42  12.2  1.04      767.     344.
#>  5 theta[3]  4.22   4.52  5.03  4.63 -4.32  12.2  1.02      554.     246.
#>  6 theta[4]  4.79   4.95  4.45  4.65 -2.24  12.0  0.998     656.     370.
#>  7 theta[5]  3.75   3.85  4.89  4.25 -4.80  11.1  1.000     609.     326.
#>  8 theta[6]  4.28   4.36  4.88  4.65 -4.21  12.3  0.998     644.     305.
#>  9 theta[7]  6.53   6.18  5.38  4.52 -0.763 15.9  0.995     624.     345.
#> 10 theta[8]  5.00   4.52  5.21  4.55 -2.83  13.3  1.01      618.     332.
```

Basically, we get a data frame with one row per variable and one column
per summary statistic or convergence diagnostic. We can choose which
summaries to compute via the `measures` argument. For instance, if we
only wanted the mean and its corresponding Monto Carlo Standard Error
(MCSE) we would use:

``` r
summarise_draws(eight_schools_df, measures = c("mean", "mcse_mean"))
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
same name as the name specified in `measures` and that takes a vector or
matrix of numeric values and returns a single numeric value or a named
vector of numeric values.

#### Subsetting draws

Another common task when working with posterior (or prior) draws, is
subsetting according to various aspects of the draws (iterations,
chains, or variables). **posterior** provides a convienent interface for
this purpose via the `subset()` method. For example, here is the code to
extract the first five iterations of the first two chains of the
variable `mu`:

``` r
subset(eight_schools_df, variable = "mu", chain = 1:2, iteration = 1:5)
#> # A tibble: 10 x 4
#>    .chain .iteration .draw    mu
#>     <int>      <int> <int> <dbl>
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

The same call to `subset()` can be used regardless of whether the object
is a `draws_df`, `draws_array`, `draws_list`, etc.

#### Converting from regular R objects to draws formats

The `eight_schools` example already comes in a format natively supported
by posterior but we could of course also import the draws from other
sources, for example, from common base R objects:

``` r
x <- matrix(rnorm(50), nrow = 10, ncol = 5)
colnames(x) <- paste0("V", 1:5)
x <- as_draws_matrix(x)
str(x)
#>  'draws_matrix' num [1:10, 1:5] -2.769 -0.312 -0.119 -0.231 1.34 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:10] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:5] "V1" "V2" "V3" "V4" ...

summarise_draws(x, c("mean", "sd", "median", "mad"))
#> # A tibble: 5 x 5
#>   variable    mean    sd  median   mad
#>   <chr>      <dbl> <dbl>   <dbl> <dbl>
#> 1 V1       -0.199  1.09  -0.122  0.592
#> 2 V2       -0.152  1.03  -0.390  0.690
#> 3 V3        0.0621 0.720 -0.0492 0.797
#> 4 V4       -0.497  1.30  -0.247  0.298
#> 5 V5       -0.384  0.659 -0.265  0.632
```

Instead of `as_draws_matrix()` we also could have just used
`as_draws()`, which attempts to find the closest available format to the
input object. In this case this would result in a `draws_matrix` object
either way.

### Contributing

We welcome contributions\! The **posterior** package is under active
development. If you find bugs or have ideas for new features (for us or
yourself to implement) please open an issue on GitHub
(<https://github.com/jgabry/posterior/issues>).

### References

Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki
Vehtari and Donald B. Rubin (2013). Bayesian Data Analysis, Third
Edition. Chapman and Hall/CRC.

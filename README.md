
<!-- README.md is generated from README.Rmd. Please edit that file -->

# posterior <img src="man/figures/stanlogo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/posterior)](https://CRAN.R-project.org/package=posterior)
[![R-CMD-check](https://github.com/stan-dev/posterior/workflows/R-CMD-check/badge.svg)](https://github.com/stan-dev/posterior/actions?workflow=R-CMD-check)
[![Coverage
Status](https://codecov.io/gh/stan-dev/posterior/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/posterior)
<!-- badges: end -->

The **posterior** R package is intended to provide useful tools for both
users and developers of packages for fitting Bayesian models or working
with output from Bayesian models. The primary goals of the package are
to:

-   Efficiently convert between many different useful formats of draws
    (samples) from posterior or prior distributions.
-   Provide consistent methods for operations commonly performed on
    draws, for example, subsetting, binding, or mutating draws.
-   Provide various summaries of draws in convenient formats.
-   Provide lightweight implementations of state of the art posterior
    inference diagnostics.

If you are new to **posterior** we recommend starting with these
vignettes:

-   [*The posterior R
    package*](https://mc-stan.org/posterior/articles/posterior.html): an
    introduction to the package and its main functionality
-   [*rvar: The Random Variable
    Datatype*](https://mc-stan.org/posterior/articles/rvar.html): an
    overview of the new random variable datatype

### Installation

The package is not on CRAN yet, but you can install the latest beta
release version via

``` r
install.packages("posterior", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

or the latest development version from GitHub via

``` r
# install.packages("remotes")
remotes::install_github("stan-dev/posterior")
```

### Examples

Here we offer a few examples of using the package. For a more detailed
overview see the vignette [*The posterior R
package*](https://mc-stan.org/posterior/articles/posterior.html).

``` r
library("posterior")
#> This is posterior version 0.1.5
#> 
#> Attaching package: 'posterior'
#> The following objects are masked from 'package:stats':
#> 
#>     mad, sd, var
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
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

Different formats are preferable in different situations and hence
posterior supports multiple formats and easy conversion between them.
For more details on the available formats see `help("draws")`. All of
the formats are essentially base R object classes and can be used as
such. For example, a `draws_matrix` object is just a `matrix` with a
little more consistency and additional methods.

#### Summarizing draws

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
per summary statistic or convergence diagnostic. The summaries `rhat`,
`ess_bulk`, and `ess_tail` are described in Vehtari et al. (2020). We
can choose which summaries to compute by passing additional arguments,
either functions or names of functions. For instance, if we only wanted
the mean and its corresponding Monte Carlo Standard Error (MCSE) we
would use:

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
chains, or variables). **posterior** provides a convenient interface for
this purpose via the `subset_draws()` method. For example, here is the
code to extract the first five iterations of the first two chains of the
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
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

The same call to `subset_draws()` can be used regardless of whether the
object is a `draws_df`, `draws_array`, `draws_list`, etc.

#### Mutating and renaming draws

The magic of having obtained draws from the joint posterior (or prior)
distribution of a set of variables is that these draws can also be used
to obtain draws from any other variable that is a function of the
original variables. That is, if are interested in the posterior
distribution of, say, `phi = (mu + tau)^2` all we have to do is to
perform the transformation for each of the individual draws to obtain
draws from the posterior distribution of the transformed variable. This
procedure is automated in the `mutate_variables` method:

``` r
x <- mutate_variables(eight_schools_df, phi = (mu + tau)^2)
x <- subset_draws(x, c("mu", "tau", "phi"))
print(x)
#> # A draws_df: 100 iterations, 4 chains, and 3 variables
#>      mu tau   phi
#> 1  2.01 2.8  22.8
#> 2  1.46 7.0  71.2
#> 3  5.81 9.7 240.0
#> 4  6.85 4.8 135.4
#> 5  1.81 2.8  21.7
#> 6  3.84 4.1  62.8
#> 7  5.47 4.0  88.8
#> 8  1.20 1.5   7.1
#> 9  0.15 3.9  16.6
#> 10 7.17 1.8  79.9
#> # ... with 390 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
```

When we do the math ourselves, we see that indeed for each draw, `phi`
is equal to `(mu + tau)^2` (up to rounding two 2 digits for the purpose
of printing).

We may also easily rename variables, or even entire vectors of variables
via `rename_variables`, for example:

``` r
x <- rename_variables(eight_schools_df, mean = mu, alpha = theta)
variables(x)
#>  [1] "mean"     "tau"      "alpha[1]" "alpha[2]" "alpha[3]" "alpha[4]" "alpha[5]"
#>  [8] "alpha[6]" "alpha[7]" "alpha[8]"
```

As with all **posterior** methods, `mutate_variables` and
`rename_variables` can be used with all draws formats.

#### Binding draws together

Suppose we have multiple draws objects that we want to bind together:

``` r
x1 <- draws_matrix(alpha = rnorm(5), beta = 1)
x2 <- draws_matrix(alpha = rnorm(5), beta = 2)
x3 <- draws_matrix(theta = rexp(5))
```

Then, we can use the `bind_draws` method to bind them along different
dimensions. For example, we can bind `x1` and `x3` together along the
`'variable'` dimension:

``` r
x4 <- bind_draws(x1, x3, along = "variable")
print(x4)
#> # A draws_matrix: 5 draws, and 3 variables
#>     variable
#> draw alpha beta theta
#>    1 -2.05    1  0.12
#>    2 -1.47    1  0.77
#>    3  0.44    1  1.38
#>    4 -2.89    1  0.17
#>    5 -0.48    1  2.07
```

Or, we can bind `x1` and `x2` together along the `'draw'` dimension:

``` r
x5 <- bind_draws(x1, x2, along = "draw")
print(x5)
#> # A draws_matrix: 10 draws, and 2 variables
#>     variable
#> draw alpha beta
#>   1  -2.05    1
#>   2  -1.47    1
#>   3   0.44    1
#>   4  -2.89    1
#>   5  -0.48    1
#>   6  -0.41    2
#>   7   1.90    2
#>   8   0.41    2
#>   9  -0.98    2
#>   10  0.37    2
```

As with all **posterior** methods, `bind_draws` can be used with all
draws formats.

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
#> draw    V1    V2     V3    V4    V5
#>   1   0.67  0.44 -0.615  1.22 -0.82
#>   2   0.35 -0.42 -0.400  1.26  0.18
#>   3   0.55  0.64  0.230 -1.45 -0.58
#>   4   1.04  0.70 -0.789  1.53 -0.65
#>   5  -1.04  0.79  0.362 -1.10 -1.43
#>   6  -0.19  0.58  1.005 -0.34 -1.46
#>   7  -0.98 -0.21 -0.963  0.57 -0.06
#>   8   0.18  0.85 -0.098 -0.40 -0.66
#>   9  -0.53 -0.65 -0.289 -1.11 -0.58
#>   10  0.65 -0.84 -0.826  1.79  0.31

summarise_draws(x, "mean", "sd", "median", "mad")
#> # A tibble: 5 x 5
#>   variable    mean    sd median   mad
#>   <chr>      <dbl> <dbl>  <dbl> <dbl>
#> 1 V1        0.0713 0.725  0.268 0.640
#> 2 V2        0.188  0.647  0.513 0.454
#> 3 V3       -0.238  0.623 -0.344 0.687
#> 4 V4        0.198  1.22   0.111 1.75 
#> 5 V5       -0.575  0.598 -0.618 0.561
```

Instead of `as_draws_matrix()` we also could have just used
`as_draws()`, which attempts to find the closest available format to the
input object. In this case this would result in a `draws_matrix` object
either way.

### Contributing to posterior

We welcome contributions! The **posterior** package is under active
development. If you find bugs or have ideas for new features (for us or
yourself to implement) please open an issue on GitHub
(<https://github.com/stan-dev/posterior/issues>).

### Citing posterior

Developing and maintaining open source software is an important yet
often underappreciated contribution to scientific progress. Thus,
whenever you are using open source software (or software in general),
please make sure to cite it appropriately so that developers get credit
for their work.

When using **posterior**, please cite it as follows:

-   Bürkner P. C., Gabry J., Kay M., & Vehtari A. (2020). “posterior:
    Tools for Working with Posterior Distributions.” R package version
    XXX, &lt;URL: <https://mc-stan.org/posterior>&gt;.

When using the MCMC convergence diagnostics `rhat`, `ess_bulk`, or
`ess_tail`, please also cite

-   Vehtari A., Gelman A., Simpson D., Carpenter B., & Bürkner P. C.
    (2020). Rank-normalization, folding, and localization: An improved
    Rhat for assessing convergence of MCMC. *Bayesian Analysis*.

The same information can be obtained by running `citation("posterior")`.

### References

Gelman A., Carlin J. B., Stern H. S., David B. Dunson D. B., Aki Vehtari
A., & Rubin D. B. (2013). *Bayesian Data Analysis, Third Edition*.
Chapman and Hall/CRC.

Vehtari A., Gelman A., Simpson D., Carpenter B., & Bürkner P. C. (2020).
Rank-normalization, folding, and localization: An improved Rhat for
assessing convergence of MCMC. *Bayesian Analysis*.

### Licensing

The **posterior** package is licensed under the following licenses:

- Code: BSD 3-clause (https://opensource.org/licenses/BSD-3-Clause)
- Documentation: CC-BY 4.0 (https://creativecommons.org/licenses/by/4.0/)
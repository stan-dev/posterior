# The posterior R package

## Introduction

The posterior R package is intended to provide useful tools for both
users and developers of packages for fitting Bayesian models or working
with output from Bayesian models. The primary goals of the package are
to:

- Efficiently convert between many different useful formats of draws
  (samples) from posterior or prior distributions.
- Provide consistent methods for operations commonly performed on draws,
  for example, subsetting, binding, or mutating draws.
- Provide various summaries of draws in convenient formats.
- Provide lightweight implementations of state of the art posterior
  inference diagnostics.

## Installation

You can install the latest official release version via

``` r
install.packages("posterior")
```

or the latest development version from GitHub via

``` r
# install.packages("remotes")
remotes::install_github("stan-dev/posterior")
```

## Example

``` r
library("posterior")
```

    ## This is posterior version 1.6.1.9000

    ## 
    ## Attaching package: 'posterior'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     mad, sd, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     %in%, match

To demonstrate how to work with the posterior package, throughout the
rest of this vignette we will use example posterior draws obtained from
the eight schools hierarchical meta-analysis model described in Gelman
et al. (2013). The variables are an estimate per school (`theta[1]`
through `theta[8]`) as well as an overall mean (`mu`) and standard
deviation across schools (`tau`).

``` r
eight_schools_array <- example_draws("eight_schools")
print(eight_schools_array, max_variables = 3)
```

    ## # A draws_array: 100 iterations, 4 chains, and 10 variables
    ## , , variable = mu
    ## 
    ##          chain
    ## iteration   1    2     3   4
    ##         1 2.0  3.0  1.79 6.5
    ##         2 1.5  8.2  5.99 9.1
    ##         3 5.8 -1.2  2.56 0.2
    ##         4 6.8 10.9  2.79 3.7
    ##         5 1.8  9.8 -0.03 5.5
    ## 
    ## , , variable = tau
    ## 
    ##          chain
    ## iteration   1    2    3   4
    ##         1 2.8 2.80  8.7 3.8
    ##         2 7.0 2.76  2.9 6.8
    ##         3 9.7 0.57  8.4 5.3
    ##         4 4.8 2.45  4.4 1.6
    ##         5 2.8 2.80 11.0 3.0
    ## 
    ## , , variable = theta[1]
    ## 
    ##          chain
    ## iteration     1     2    3     4
    ##         1  3.96  6.26 13.3  5.78
    ##         2  0.12  9.32  6.3  2.09
    ##         3 21.25 -0.97 10.6 15.72
    ##         4 14.70 12.45  5.4  2.69
    ##         5  5.96  9.75  8.2 -0.91
    ## 
    ## # ... with 95 more iterations, and 7 more variables

The structure of this object is explained in the next section.

## Draws formats

### Available formats

Because different formats are preferable in different situations,
posterior supports multiple formats and easy conversion between them.
The currently supported formats are:

- `draws_array`: An iterations by chains by variables array.
- `draws_matrix`: A draws (iterations x chains) by variables array.
- `draws_df`: A draws by variables data frame with addition meta columns
  `.chain`, `.iteration`, `.draw`.
- `draws_list`: A list with one sublist per chain. Each sublist is a
  named list with one vector of iterations per variable.
- `draws_rvars`: A list of random variable `rvar` objects, one per
  variable. See
  [`vignette("rvar")`](https://mc-stan.org/posterior/dev/articles/rvar.md)
  for an introduction to this new data type.

These formats are essentially base R object classes and can be used as
such. For example, a `draws_matrix` object is just a `matrix` with a
little more consistency (e.g., no dropping of dimensions with one level
when indexing) and additional methods. The exception to this is the
`draws_rvars` format, which contains `rvar` objects that behave somewhat
like arrays but are really a unique data type. See the separate vignette
on the `rvar` and `draws_rvars` data types for details.

The draws for our example come as a `draws_array` object with 100
iterations, 4 chains, and 10 variables:

``` r
str(eight_schools_array)
```

    ##  'draws_array' num [1:100, 1:4, 1:10] 2.01 1.46 5.81 6.85 1.81 ...
    ##  - attr(*, "dimnames")=List of 3
    ##   ..$ iteration: chr [1:100] "1" "2" "3" "4" ...
    ##   ..$ chain    : chr [1:4] "1" "2" "3" "4"
    ##   ..$ variable : chr [1:10] "mu" "tau" "theta[1]" "theta[2]" ...

### Converting between formats

Each of the formats has a method `as_draws_<format>` (e.g.,
[`as_draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md))
for creating an object of the class from any of the other formats. As a
demonstration we can convert the example `draws_array` to a `draws_df`,
a data frame with additional meta information. To convert to a
`draws_df` we use
[`as_draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md).

``` r
eight_schools_df <- as_draws_df(eight_schools_array)
str(eight_schools_df)
```

    ## draws_df [400 × 13] (S3: draws_df/draws/tbl_df/tbl/data.frame)
    ##  $ mu        : num [1:400] 2.01 1.46 5.81 6.85 1.81 ...
    ##  $ tau       : num [1:400] 2.77 6.98 9.68 4.79 2.85 ...
    ##  $ theta[1]  : num [1:400] 3.962 0.124 21.251 14.7 5.96 ...
    ##  $ theta[2]  : num [1:400] 0.271 -0.069 14.931 8.586 1.156 ...
    ##  $ theta[3]  : num [1:400] -0.743 0.952 1.829 2.675 3.109 ...
    ##  $ theta[4]  : num [1:400] 2.1 7.28 1.38 4.39 1.99 ...
    ##  $ theta[5]  : num [1:400] 0.923 -0.062 0.531 4.758 0.769 ...
    ##  $ theta[6]  : num [1:400] 1.65 11.26 7.16 8.1 4.66 ...
    ##  $ theta[7]  : num [1:400] 3.32 9.62 14.8 9.49 1.21 ...
    ##  $ theta[8]  : num [1:400] 4.85 -8.64 -1.74 5.28 -4.54 ...
    ##  $ .chain    : int [1:400] 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ .iteration: int [1:400] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ .draw     : int [1:400] 1 2 3 4 5 6 7 8 9 10 ...

``` r
print(eight_schools_df)
```

    ## # A draws_df: 100 iterations, 4 chains, and 10 variables
    ##      mu tau theta[1] theta[2] theta[3] theta[4] theta[5] theta[6]
    ## 1  2.01 2.8     3.96    0.271    -0.74      2.1    0.923      1.7
    ## 2  1.46 7.0     0.12   -0.069     0.95      7.3   -0.062     11.3
    ## 3  5.81 9.7    21.25   14.931     1.83      1.4    0.531      7.2
    ## 4  6.85 4.8    14.70    8.586     2.67      4.4    4.758      8.1
    ## 5  1.81 2.8     5.96    1.156     3.11      2.0    0.769      4.7
    ## 6  3.84 4.1     5.76    9.909    -1.00      5.3    5.889     -1.7
    ## 7  5.47 4.0     4.03    4.151    10.15      6.6    3.741     -2.2
    ## 8  1.20 1.5    -0.28    1.846     0.47      4.3    1.467      3.3
    ## 9  0.15 3.9     1.81    0.661     0.86      4.5   -1.025      1.1
    ## 10 7.17 1.8     6.08    8.102     7.68      5.6    7.106      8.5
    ## # ... with 390 more draws, and 2 more variables
    ## # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

### Converting regular R objects to `draws` formats

The example draws already come in a format natively supported by
posterior, but we can of course also import the draws from other sources
like common base R objects.

#### Example: create draws_matrix from a matrix

In addition to converting other `draws` objects to the `draws_matrix`
format, the
[`as_draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
function will convert a regular matrix to a `draws_matrix`.

``` r
x <- matrix(rnorm(50), nrow = 10, ncol = 5)
colnames(x) <- paste0("V", 1:5)
x <- as_draws_matrix(x)
print(x)
```

    ## # A draws_matrix: 10 iterations, 1 chains, and 5 variables
    ##     variable
    ## draw      V1     V2     V3    V4     V5
    ##   1  -1.4000 -0.554  0.468  0.94  0.070
    ##   2   0.2553  0.629  0.363  0.18 -0.639
    ##   3  -2.4373  2.065 -1.305  0.24 -0.050
    ##   4  -0.0056 -1.631  0.738  1.62 -0.251
    ##   5   0.6216  0.512  1.889  0.11  0.445
    ##   6   1.1484 -1.863 -0.097 -0.13  2.755
    ##   7  -1.8218 -0.522 -0.936 -1.91  0.047
    ##   8  -0.2473 -0.053 -0.016 -0.28  0.578
    ##   9  -0.2442  0.543 -0.827 -0.31  0.118
    ##   10 -0.2827 -0.914 -1.512  1.07 -1.912

Because the matrix was converted to a `draws_matrix`, all of the methods
for working with `draws` objects described in subsequent sections of
this vignette will now be available.

Instead of
[`as_draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
we also could have just used
[`as_draws()`](https://mc-stan.org/posterior/dev/reference/draws.md),
which attempts to find the closest available format to the input object.
In this case the result would be a `draws_matrix` object either way.

#### Example: create draws_matrix from multiple vectors

In addition to the
[`as_draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
converter function there is also a
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md)
constructor function that can be used to create draws matrix from
multiple vectors.

``` r
x <- draws_matrix(alpha = rnorm(50), beta = rnorm(50))
print(x)
```

    ## # A draws_matrix: 50 iterations, 1 chains, and 2 variables
    ##     variable
    ## draw  alpha  beta
    ##   1   0.862 -0.39
    ##   2  -0.243 -0.79
    ##   3  -0.206 -1.06
    ##   4   0.019 -0.80
    ##   5   0.030 -1.76
    ##   6   0.550 -0.69
    ##   7  -2.274 -0.56
    ##   8   2.683 -0.54
    ##   9  -0.361  0.23
    ##   10  0.213  0.98
    ## # ... with 40 more draws

Analogous functions exist for the other draws formats and are used
similarly.

## Manipulating `draws` objects

The posterior package provides many methods for manipulating draws
objects in useful ways. In this section we demonstrate several of the
most commonly used methods. These methods, like the other methods in
posterior, are available for every supported draws format.

### Subsetting

Subsetting `draws` objects can be done according to various aspects of
the draws (iterations, chains, or variables). The posterior package
provides a convenient interface for this purpose via
[`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md).
For example, here is the code to extract the first five iterations of
the first two chains of the variable `mu`.

``` r
sub_df <- subset_draws(eight_schools_df, variable = "mu", chain = 1:2, iteration = 1:5)
str(sub_df)
```

    ## draws_df [10 × 4] (S3: draws_df/draws/tbl_df/tbl/data.frame)
    ##  $ mu        : num [1:10] 2.01 1.46 5.81 6.85 1.81 ...
    ##  $ .chain    : int [1:10] 1 1 1 1 1 2 2 2 2 2
    ##  $ .iteration: int [1:10] 1 2 3 4 5 1 2 3 4 5
    ##  $ .draw     : int [1:10] 1 2 3 4 5 6 7 8 9 10

The same call to
[`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
can be used regardless of the draws format. For example, here is the
same code except replacing the `draws_df` object with the `draws_array`
object.

``` r
sub_arr <- subset_draws(eight_schools_array, variable = "mu", chain = 1:2, iteration = 1:5)
str(sub_arr)
```

    ##  'draws_array' num [1:5, 1:2, 1] 2.01 1.46 5.81 6.85 1.81 ...
    ##  - attr(*, "dimnames")=List of 3
    ##   ..$ iteration: chr [1:5] "1" "2" "3" "4" ...
    ##   ..$ chain    : chr [1:2] "1" "2"
    ##   ..$ variable : chr "mu"

We can check that these two calls to
[`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
(the first with the data frame, the second with the array) produce the
same result.

``` r
identical(sub_df, as_draws_df(sub_arr))
identical(as_draws_array(sub_df), sub_arr)
```

    ## [1] TRUE
    ## [1] TRUE

It is also possible to use standard R subsetting syntax with `draws`
objects. The following is equivalent to the use of
[`subset_draws()`](https://mc-stan.org/posterior/dev/reference/subset_draws.md)
with the array above.

``` r
eight_schools_array[1:5, 1:2, "mu"]
```

    ## # A draws_array: 5 iterations, 2 chains, and 1 variables
    ## , , variable = mu
    ## 
    ##          chain
    ## iteration   1    2
    ##         1 2.0  3.0
    ##         2 1.5  8.2
    ##         3 5.8 -1.2
    ##         4 6.8 10.9
    ##         5 1.8  9.8

The major difference between how posterior behaves when indexing and how
base R behaves is that posterior will *not* drop dimensions with only
one level. That is, even though there is only one variable left after
subsetting, the result of the subsetting above is still a `draws_array`
and not a `draws_matrix`.

### Mutating (transformations of variables)

The magic of having obtained draws from the joint posterior (or prior)
distribution of a set of variables is that these draws can also be used
to obtain draws from any other variable that is a function of the
original variables. That is, if we are interested in the posterior
distribution of, say, `phi = (mu + tau)^2` all we have to do is to
perform the transformation for each of the individual draws to obtain
draws from the posterior distribution of the transformed variable. This
procedure is handled by
[`mutate_variables()`](https://mc-stan.org/posterior/dev/reference/mutate_variables.md).

``` r
x <- mutate_variables(eight_schools_df, phi = (mu + tau)^2)
x <- subset_draws(x, c("mu", "tau", "phi"))
print(x)
```

    ## # A draws_df: 100 iterations, 4 chains, and 3 variables
    ##      mu tau   phi
    ## 1  2.01 2.8  22.8
    ## 2  1.46 7.0  71.2
    ## 3  5.81 9.7 240.0
    ## 4  6.85 4.8 135.4
    ## 5  1.81 2.8  21.7
    ## 6  3.84 4.1  62.8
    ## 7  5.47 4.0  88.8
    ## 8  1.20 1.5   7.1
    ## 9  0.15 3.9  16.6
    ## 10 7.17 1.8  79.9
    ## # ... with 390 more draws
    ## # ... hidden reserved variables {'.chain', '.iteration', '.draw'}

### Renaming

To rename variables use
[`rename_variables()`](https://mc-stan.org/posterior/dev/reference/rename_variables.md).
Here we rename the scalar `mu` to `mean` and the vector `theta` to
`alpha`.

``` r
# mu is a scalar, theta is a vector
x <- rename_variables(eight_schools_df, mean = mu, alpha = theta)
variables(x)
```

    ##  [1] "mean"     "tau"      "alpha[1]" "alpha[2]" "alpha[3]" "alpha[4]"
    ##  [7] "alpha[5]" "alpha[6]" "alpha[7]" "alpha[8]"

In the call to
[`rename_variables()`](https://mc-stan.org/posterior/dev/reference/rename_variables.md)
above, `mu` and `theta` can be quoted or unquoted.

It is also possible to rename individual elements of non-scalar
parameters, for example we can rename just the first element of `alpha`:

``` r
x <- rename_variables(x, a1 = `alpha[1]`)
variables(x)
```

    ##  [1] "mean"     "tau"      "a1"       "alpha[2]" "alpha[3]" "alpha[4]"
    ##  [7] "alpha[5]" "alpha[6]" "alpha[7]" "alpha[8]"

### Binding

The
[`bind_draws()`](https://mc-stan.org/posterior/dev/reference/bind_draws.md)
method can be used to combine `draws` objects along different
dimensions. As an example, suppose we have several different
`draws_matrix` objects:

``` r
x1 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
x2 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
x3 <- draws_matrix(theta = rexp(5))
```

We can bind `x1` and `x3` together along the `'variable'` dimension to
get a single `draws_matrix` with the variables from both `x1` and `x3`:

``` r
x4 <- bind_draws(x1, x3, along = "variable")
print(x4)
```

    ## # A draws_matrix: 5 iterations, 1 chains, and 3 variables
    ##     variable
    ## draw alpha   beta theta
    ##    1  0.43  0.678  0.81
    ##    2  0.12  0.038  0.13
    ##    3 -1.14 -0.356  0.41
    ##    4 -0.56  0.783  0.19
    ##    5  1.05  0.804  0.56

Because `x1` and `x2` have the same variables, we can bind them along
the `'draw'` dimension to create a single `draws_matrix` with more
draws:

``` r
x5 <- bind_draws(x1, x2, along = "draw")
print(x5)
```

    ## # A draws_matrix: 10 iterations, 1 chains, and 2 variables
    ##     variable
    ## draw alpha   beta
    ##   1   0.43  0.678
    ##   2   0.12  0.038
    ##   3  -1.14 -0.356
    ##   4  -0.56  0.783
    ##   5   1.05  0.804
    ##   6  -1.90 -0.788
    ##   7   0.94 -1.133
    ##   8  -0.31  0.364
    ##   9   0.26 -0.286
    ##   10 -1.79  0.518

As with all posterior methods,
[`bind_draws()`](https://mc-stan.org/posterior/dev/reference/bind_draws.md)
can be used with all draws formats and depending on the format different
dimensions are available to bind on. For example, we can bind
`draws_array` objects together by `iteration`, `chain`, or `variable`,
but a 2-D `draws_matrix` with the chains combined can only by bound by
`draw` and `variable`.

## Summaries and diagnostics

### summarise_draws() basic usage

Computing summaries of posterior or prior draws and convergence
diagnostics for posterior draws are some of the most common tasks when
working with Bayesian models fit using Markov Chain Monte Carlo (MCMC)
methods. The posterior package provides a flexible interface for this
purpose via
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
(or
[`summarize_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)),
which can be passed any of the formats supported by the package.

``` r
# summarise_draws or summarize_draws
summarise_draws(eight_schools_df)
```

    ## # A tibble: 10 × 10
    ##    variable  mean median    sd   mad      q5   q95  rhat ess_bulk ess_tail
    ##    <chr>    <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl> <dbl>    <dbl>    <dbl>
    ##  1 mu        4.18   4.16  3.40  3.57  -0.854  9.39  1.02     558.     322.
    ##  2 tau       4.16   3.07  3.58  2.89   0.309 11.0   1.01     246.     202.
    ##  3 theta[1]  6.75   5.97  6.30  4.87  -1.23  18.9   1.01     400.     254.
    ##  4 theta[2]  5.25   5.13  4.63  4.25  -1.97  12.5   1.02     564.     372.
    ##  5 theta[3]  3.04   3.99  6.80  4.94 -10.3   11.9   1.01     312.     205.
    ##  6 theta[4]  4.86   4.99  4.92  4.51  -3.57  12.2   1.02     695.     252.
    ##  7 theta[5]  3.22   3.72  5.08  4.38  -5.93  10.8   1.01     523.     306.
    ##  8 theta[6]  3.99   4.14  5.16  4.81  -4.32  11.5   1.02     548.     205.
    ##  9 theta[7]  6.50   5.90  5.26  4.54  -1.19  15.4   1.00     434.     308.
    ## 10 theta[8]  4.57   4.64  5.25  4.89  -3.79  12.2   1.02     355.     146.

The result is a data frame with one row per variable and one column per
summary statistic or convergence diagnostic. The summaries `rhat`,
`ess_bulk`, and `ess_tail` are described in Vehtari et al. (2020). We
can choose which summaries to compute by passing additional arguments,
either functions or names of functions. For instance, if we only wanted
the mean and its corresponding Monte Carlo Standard Error (MCSE) we
could use either of these options:

``` r
# the function mcse_mean is provided by the posterior package
s1 <- summarise_draws(eight_schools_df, "mean", "mcse_mean") 
s2 <- summarise_draws(eight_schools_df, mean, mcse_mean) 
identical(s1, s2)
```

    ## [1] TRUE

``` r
print(s1)
```

    ## # A tibble: 10 × 3
    ##    variable  mean mcse_mean
    ##    <chr>    <dbl>     <dbl>
    ##  1 mu        4.18     0.150
    ##  2 tau       4.16     0.213
    ##  3 theta[1]  6.75     0.319
    ##  4 theta[2]  5.25     0.202
    ##  5 theta[3]  3.04     0.447
    ##  6 theta[4]  4.86     0.189
    ##  7 theta[5]  3.22     0.232
    ##  8 theta[6]  3.99     0.222
    ##  9 theta[7]  6.50     0.250
    ## 10 theta[8]  4.57     0.273

### Changing column names

The column names in the output can be changed by providing the functions
as name-value pairs, where the name is the name to use in the output and
the value is a function name or definition. For example, here we change
the names `mean` and `sd` to `posterior_mean` and `posterior_sd`.

``` r
summarise_draws(eight_schools_df, posterior_mean = mean, posterior_sd = sd)
```

    ## # A tibble: 10 × 3
    ##    variable posterior_mean posterior_sd
    ##    <chr>             <dbl>        <dbl>
    ##  1 mu                 4.18         3.40
    ##  2 tau                4.16         3.58
    ##  3 theta[1]           6.75         6.30
    ##  4 theta[2]           5.25         4.63
    ##  5 theta[3]           3.04         6.80
    ##  6 theta[4]           4.86         4.92
    ##  7 theta[5]           3.22         5.08
    ##  8 theta[6]           3.99         5.16
    ##  9 theta[7]           6.50         5.26
    ## 10 theta[8]           4.57         5.25

### Using custom functions

For a function to work with
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md),
it needs to take a vector or matrix of numeric values and return a
single numeric value or a named vector of numeric values. Additional
arguments to the function can be specified in a list passed to the
`.args` argument.

``` r
weighted_mean <- function(x, wts) {
  sum(x * wts)/sum(wts)
}
summarise_draws(
  eight_schools_df, 
  weighted_mean, 
  .args = list(wts = rexp(ndraws(eight_schools_df)))
)
```

    ## # A tibble: 10 × 2
    ##    variable weighted_mean
    ##    <chr>            <dbl>
    ##  1 mu                4.30
    ##  2 tau               4.50
    ##  3 theta[1]          7.43
    ##  4 theta[2]          5.55
    ##  5 theta[3]          2.77
    ##  6 theta[4]          5.15
    ##  7 theta[5]          3.22
    ##  8 theta[6]          3.84
    ##  9 theta[7]          7.05
    ## 10 theta[8]          4.80

### Specifying functions using lambda-like syntax

It is also possible to specify a summary function using a one-sided
formula that follows the conventions supported by
[`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).
For example, the function

``` r
function(x) quantile(x, probs = c(0.4, 0.6))
```

can be simplified to

``` r
# for multiple arguments `.x` and `.y` can be used, see ?rlang::as_function
~quantile(., probs = c(0.4, 0.6))
```

Both can be used with
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
and produce the same output:

``` r
summarise_draws(eight_schools_df, function(x) quantile(x, probs = c(0.4, 0.6)))
```

    ## # A tibble: 10 × 3
    ##    variable `40%` `60%`
    ##    <chr>    <dbl> <dbl>
    ##  1 mu        3.41  5.35
    ##  2 tau       2.47  3.96
    ##  3 theta[1]  4.95  7.01
    ##  4 theta[2]  4.32  6.13
    ##  5 theta[3]  2.54  5.33
    ##  6 theta[4]  3.78  6.11
    ##  7 theta[5]  2.69  4.69
    ##  8 theta[6]  2.92  5.47
    ##  9 theta[7]  4.81  7.33
    ## 10 theta[8]  3.50  5.92

``` r
summarise_draws(eight_schools_df, ~quantile(.x, probs = c(0.4, 0.6)))
```

    ## # A tibble: 10 × 3
    ##    variable `40%` `60%`
    ##    <chr>    <dbl> <dbl>
    ##  1 mu        3.41  5.35
    ##  2 tau       2.47  3.96
    ##  3 theta[1]  4.95  7.01
    ##  4 theta[2]  4.32  6.13
    ##  5 theta[3]  2.54  5.33
    ##  6 theta[4]  3.78  6.11
    ##  7 theta[5]  2.69  4.69
    ##  8 theta[6]  2.92  5.47
    ##  9 theta[7]  4.81  7.33
    ## 10 theta[8]  3.50  5.92

See
[`help("as_function", "rlang")`](https://rlang.r-lib.org/reference/as_function.html)
for details on specifying these functions.

### Other diagnostics

In addition to the default diagnostic functions used by
[`summarise_draws()`](https://mc-stan.org/posterior/dev/reference/draws_summary.md)
([`rhat()`](https://mc-stan.org/posterior/dev/reference/rhat.md),
[`ess_bulk()`](https://mc-stan.org/posterior/dev/reference/ess_bulk.md),
[`ess_tail()`](https://mc-stan.org/posterior/dev/reference/ess_tail.md)),
posterior also provides additional diagnostics like effective sample
sizes and Monte Carlo standard errors for quantiles and standard
deviations, an experimental new diagnostic called R\*, and others. For a
list of available diagnostics and links to their individual help pages
see
[`help("diagnostics", "posterior")`](https://mc-stan.org/posterior/dev/reference/diagnostics.md).

If you have suggestions for additional diagnostics that should be
implemented in posterior, please open an issue at
<https://github.com/stan-dev/posterior/issues>.

## Other methods for working with `draws` objects

In addition to the methods demonstrated in this vignette, posterior has
various other methods available for working with `draws` objects. The
following is a (potentially incomplete) list.

| **Method**                                                                                            | **Description**                                                                                                   |
|:------------------------------------------------------------------------------------------------------|:------------------------------------------------------------------------------------------------------------------|
| [`order_draws()`](https://mc-stan.org/posterior/dev/reference/order_draws.md)                         | Order `draws` objects according to iteration and chain number                                                     |
| [`repair_draws()`](https://mc-stan.org/posterior/dev/reference/repair_draws.md)                       | Repair indices of `draws` objects so that iterations chains, and draws are continuously and consistently numbered |
| [`resample_draws()`](https://mc-stan.org/posterior/dev/reference/resample_draws.md)                   | Resample `draws` objects according to provided weights                                                            |
| [`thin_draws()`](https://mc-stan.org/posterior/dev/reference/thin_draws.md)                           | Thin `draws` objects to reduce size and autocorrelation                                                           |
| [`weight_draws()`](https://mc-stan.org/posterior/dev/reference/weight_draws.md)                       | Add weights to draws objects, with one weight per draw, for use in subsequent weighting operations                |
| [`extract_variable()`](https://mc-stan.org/posterior/dev/reference/extract_variable.md)               | Extract a vector of draws of a single variable                                                                    |
| [`extract_variable_matrix()`](https://mc-stan.org/posterior/dev/reference/extract_variable_matrix.md) | Extract an iterations x chains matrix of draws of a single variable                                               |
| [`merge_chains()`](https://mc-stan.org/posterior/dev/reference/merge_chains.md)                       | Merge chains of `draws` objects into a single chain.                                                              |
| [`split_chains()`](https://mc-stan.org/posterior/dev/reference/split_chains.md)                       | Split chains of `draws` objects by halving the number of iterations per chain and doubling the number of chains.  |

If you have suggestions for additional methods that would be useful for
working with `draws` objects, please open an issue at
<https://github.com/stan-dev/posterior/issues>.

## References

Gelman A., Carlin J. B., Stern H. S., David B. Dunson D. B., Vehtari,
A., & Rubin D. B. (2013). *Bayesian Data Analysis, Third Edition*.
Chapman and Hall/CRC.

Vehtari A., Gelman A., Simpson D., Carpenter B., & Bürkner P. C. (2020).
Rank-normalization, folding, and localization: An improved Rhat for
assessing convergence of MCMC. *Bayesian Analysis*, 16(2):667-718.

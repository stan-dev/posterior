# Subset `draws` objects

Subset [`draws`](https://mc-stan.org/posterior/dev/reference/draws.md)
objects by variables, iterations, chains, and draws indices.

## Usage

``` r
subset_draws(x, ...)

# S3 method for class 'draws_matrix'
subset_draws(
  x,
  variable = NULL,
  iteration = NULL,
  chain = NULL,
  draw = NULL,
  regex = FALSE,
  unique = TRUE,
  exclude = FALSE,
  scalar = FALSE,
  ...
)

# S3 method for class 'draws_array'
subset_draws(
  x,
  variable = NULL,
  iteration = NULL,
  chain = NULL,
  draw = NULL,
  regex = FALSE,
  unique = TRUE,
  exclude = FALSE,
  scalar = FALSE,
  ...
)

# S3 method for class 'draws_df'
subset_draws(
  x,
  variable = NULL,
  iteration = NULL,
  chain = NULL,
  draw = NULL,
  regex = FALSE,
  unique = TRUE,
  exclude = FALSE,
  scalar = FALSE,
  ...
)

# S3 method for class 'draws_list'
subset_draws(
  x,
  variable = NULL,
  iteration = NULL,
  chain = NULL,
  draw = NULL,
  regex = FALSE,
  unique = TRUE,
  exclude = FALSE,
  scalar = FALSE,
  ...
)

# S3 method for class 'draws_rvars'
subset_draws(
  x,
  variable = NULL,
  iteration = NULL,
  chain = NULL,
  draw = NULL,
  regex = FALSE,
  unique = TRUE,
  exclude = FALSE,
  scalar = FALSE,
  ...
)

# S3 method for class 'rvar'
subset_draws(x, variable = NULL, ...)

# S3 method for class 'draws'
subset(x, ...)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

- variable:

  (character vector) The variables to select. All elements of non-scalar
  variables can be selected at once.

- iteration:

  (integer vector) The iteration indices to select.

- chain:

  (integer vector) The chain indices to select.

- draw:

  (integer vector) The draw indices to be select. Subsetting draw
  indices will lead to an automatic merging of chains via
  [`merge_chains`](https://mc-stan.org/posterior/dev/reference/merge_chains.md).
  By default, a message will be issued when this happens but you can
  deactivate it via `options(posterior.warn_on_merge_chains = FALSE)`.

- regex:

  (logical) Should `variable` should be treated as a (vector of) regular
  expressions? Any variable in `x` matching at least one of the regular
  expressions will be selected. Defaults to `FALSE`.

- unique:

  (logical) Should duplicated selection of chains, iterations, or draws
  be allowed? If `TRUE` (the default) only unique chains, iterations,
  and draws are selected regardless of how often they appear in the
  respective selecting arguments.

- exclude:

  (logical) Should the selected subset be excluded? If `FALSE` (the
  default) only the selected subset will be returned. If `TRUE`
  everything but the selected subset will be returned.

- scalar:

  (logical) Should only scalar variables be selected? If `FALSE` (the
  default), all variables with matching names and *arbitrary* indices
  will be selected (see examples).

## Value

A `draws` object of the same class as `x`.

## Details

To ensure that multiple consecutive subsetting operations work
correctly, [`subset()`](https://rdrr.io/r/base/subset.html)
*[repairs](https://mc-stan.org/posterior/dev/reference/repair_draws.md)*
the `draws` object before and after subsetting.

## Examples

``` r
x <- example_draws()
subset_draws(x, variable = c("mu", "tau"))
#> # A draws_array: 100 iterations, 4 chains, and 2 variables
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
#> # ... with 95 more iterations
subset_draws(x, chain = 2)
#> # A draws_array: 100 iterations, 1 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration    1
#>         1  3.0
#>         2  8.2
#>         3 -1.2
#>         4 10.9
#>         5  9.8
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration    1
#>         1 2.80
#>         2 2.76
#>         3 0.57
#>         4 2.45
#>         5 2.80
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration     1
#>         1  6.26
#>         2  9.32
#>         3 -0.97
#>         4 12.45
#>         5  9.75
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration    1
#>         1  1.0
#>         2  9.4
#>         3 -1.2
#>         4 12.5
#>         5 11.9
#> 
#> # ... with 95 more iterations, and 6 more variables
subset_draws(x, iteration = 5:10, chain = 3:4)
#> # A draws_array: 6 iterations, 2 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration     1     2
#>         1 -0.03  5.48
#>         2  1.06  2.38
#>         3  3.67 11.82
#>         4  3.51  4.90
#>         5  8.85  0.88
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration     1    2
#>         1 11.03  3.0
#>         2  2.70  2.3
#>         3  1.68  4.3
#>         4  0.52  3.1
#>         5  5.96 15.8
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration    1     2
#>         1  8.2 -0.91
#>         2  5.0  0.59
#>         3  5.2 18.87
#>         4  3.7  1.50
#>         5 13.1  9.07
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration   1    2
#>         1 3.2  3.2
#>         2 4.3  1.1
#>         3 4.1 13.0
#>         4 4.1  6.1
#>         5 4.7 11.6
#> 
#> # ... with 1 more iterations, and 6 more variables

# extract the first chain twice
subset_draws(x, chain = c(1, 1), unique = FALSE)
#> # A draws_array: 100 iterations, 2 chains, and 10 variables
#> , , variable = mu
#> 
#>          chain
#> iteration   1   2
#>         1 2.0 2.0
#>         2 1.5 1.5
#>         3 5.8 5.8
#>         4 6.8 6.8
#>         5 1.8 1.8
#> 
#> , , variable = tau
#> 
#>          chain
#> iteration   1   2
#>         1 2.8 2.8
#>         2 7.0 7.0
#>         3 9.7 9.7
#>         4 4.8 4.8
#>         5 2.8 2.8
#> 
#> , , variable = theta[1]
#> 
#>          chain
#> iteration     1     2
#>         1  3.96  3.96
#>         2  0.12  0.12
#>         3 21.25 21.25
#>         4 14.70 14.70
#>         5  5.96  5.96
#> 
#> , , variable = theta[2]
#> 
#>          chain
#> iteration      1      2
#>         1  0.271  0.271
#>         2 -0.069 -0.069
#>         3 14.931 14.931
#>         4  8.586  8.586
#>         5  1.156  1.156
#> 
#> # ... with 95 more iterations, and 6 more variables

# extract all elements of 'theta'
subset_draws(x, variable = "theta")
#> # A draws_array: 100 iterations, 4 chains, and 8 variables
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
#> , , variable = theta[2]
#> 
#>          chain
#> iteration      1    2   3   4
#>         1  0.271  1.0 2.1 5.0
#>         2 -0.069  9.4 7.3 8.2
#>         3 14.931 -1.2 5.7 6.0
#>         4  8.586 12.5 2.8 2.7
#>         5  1.156 11.9 3.2 3.2
#> 
#> , , variable = theta[3]
#> 
#>          chain
#> iteration     1     2     3   4
#>         1 -0.74  0.22   1.4 5.7
#>         2  0.95  9.68   4.1 3.5
#>         3  1.83 -1.37  -8.3 3.1
#>         4  2.67 11.15 -10.8 3.2
#>         5  3.11 12.72 -27.8 2.6
#> 
#> , , variable = theta[4]
#> 
#>          chain
#> iteration   1     2    3    4
#>         1 2.1 -0.73  7.8  4.8
#>         2 7.3  8.11  4.7  7.8
#>         3 1.4 -0.79  6.8  6.1
#>         4 4.4 10.48  6.8  7.9
#>         5 2.0  9.70 12.3 11.3
#> 
#> # ... with 95 more iterations, and 4 more variables

# trying to extract only a scalar 'theta' will fail
# subset_draws(x, variable = "theta", scalar = TRUE)
```

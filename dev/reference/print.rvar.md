# Print or format a random variable

Printing and formatting methods for
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

## Usage

``` r
# S3 method for class 'rvar'
print(
  x,
  ...,
  summary = NULL,
  digits = NULL,
  color = TRUE,
  width = getOption("width")
)

# S3 method for class 'rvar'
format(x, ..., summary = NULL, digits = NULL, color = FALSE)

# S3 method for class 'rvar'
str(
  object,
  ...,
  summary = NULL,
  vec.len = NULL,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
  nest.lev = 0,
  give.attr = TRUE
)
```

## Arguments

- x, object:

  (rvar) The
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) to
  print.

- ...:

  Further arguments passed to the underlying
  [`print()`](https://rdrr.io/r/base/print.html) methods.

- summary:

  (string) The style of summary to display:

  - `"mean_sd"` displays `mean ± sd`

  - `"median_mad"` displays `median ± mad`

  - `"mode_entropy"` displays `mode <entropy>`, and is used
    automatically for
    [`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)s.
    It shows normalized entropy, which ranges from 0 (all probability in
    one category) to 1 (uniform). See
    [`entropy()`](https://mc-stan.org/posterior/dev/reference/entropy.md).

  - `"mode_dissent"` displays `mode <dissent>`, and is used
    automatically for
    [`rvar_ordered`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)s.
    It shows Tastle and Wierman's (2007) *dissention* measure, which
    ranges from 0 (all probability in one category) through 0.5
    (uniform) to 1 (bimodal: all probability split equally between the
    first and last category). See
    [`dissent()`](https://mc-stan.org/posterior/dev/reference/dissent.md).

  - `NULL` uses `getOption("posterior.rvar_summary")` (default
    `"mean_sd`)

- digits:

  (nonnegative integer) The minimum number of significant digits to
  print. If `NULL`, defaults to `getOption("posterior.digits", 2)`.

- color:

  (logical) Whether or not to use color when formatting the output. If
  `TRUE`, the
  [`pillar::style_num()`](https://pillar.r-lib.org/reference/style_subtle.html)
  functions may be used to produce strings containing control sequences
  to produce colored output on the terminal.

- width:

  The maxmimum width used to print out lists of factor levels for
  [`rvar_factor`](https://mc-stan.org/posterior/dev/reference/rvar_factor.md)s.
  See [`format()`](https://rdrr.io/r/base/format.html).

- vec.len:

  (nonnegative integer) How many 'first few' elements are displayed of
  each vector. If `NULL`, defaults to `getOption("str")$vec.len`, which
  defaults to 4.

- indent.str:

  (string) The indentation string to use.

- nest.lev:

  (nonnegative integer) Current nesting level in the recursive calls to
  [`str()`](https://rdrr.io/r/utils/str.html).

- give.attr:

  (logical) If `TRUE` (default), show attributes as sub structures.

## Value

For [`print()`](https://rdrr.io/r/base/print.html), an invisible version
of the input object.

For [`str()`](https://rdrr.io/r/utils/str.html), nothing; i.e.
`invisible(NULL)`.

For [`format()`](https://rdrr.io/r/base/format.html), a character vector
of the same dimensions as `x` where each entry is of the form
`"mean±sd"` or `"median±mad"`, depending on the value of `summary`.

## Details

[`print()`](https://rdrr.io/r/base/print.html) and
[`str()`](https://rdrr.io/r/utils/str.html) print out
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) objects by
summarizing each element in the random variable with either its mean±sd
or median±mad, depending on the value of `summary`. Both functions use
the [`format()`](https://rdrr.io/r/base/format.html) implementation for
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) objects
under the hood, which returns a character vector in the mean±sd or
median±mad form.

## References

William J. Tastle, Mark J. Wierman (2007). Consensus and dissention: A
measure of ordinal dispersion. *International Journal of Approximate
Reasoning*. 45(3), 531–545.
[doi:10.1016/j.ijar.2006.06.024](https://doi.org/10.1016/j.ijar.2006.06.024)
.

## Examples

``` r
set.seed(5678)
x = rbind(
  cbind(rvar(rnorm(1000, 1)), rvar(rnorm(1000, 2))),
  cbind(rvar(rnorm(1000, 3)), rvar(rnorm(1000, 4)))
)

print(x)
#> rvar<1000>[2,2] mean ± sd:
#>      [,1]      [,2]     
#> [1,] 1 ± 1.01  2 ± 0.99 
#> [2,] 3 ± 1.00  4 ± 1.03 
print(x, summary = "median_mad")
#> rvar<1000>[2,2] median ± mad:
#>      [,1]        [,2]       
#> [1,] 1.0 ± 1.00  2.0 ± 0.98 
#> [2,] 3.0 ± 1.03  3.9 ± 1.02 

str(x)
#>  rvar<1000>[2,2]  1 ± 1.01  3 ± 1.00  2 ± 0.99  4 ± 1.03

format(x)
#>      [,1]       [,2]      
#> [1,] "1 ± 1.01" "2 ± 0.99"
#> [2,] "3 ± 1.00" "4 ± 1.03"
```

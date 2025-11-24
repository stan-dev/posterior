# Random variable ifelse

A version of [`ifelse()`](https://rdrr.io/r/base/ifelse.html) that
returns an
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

## Usage

``` r
rvar_ifelse(test, yes, no)
```

## Arguments

- test:

  (logical
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), or
  castable to one) logical test determining whether the value in `yes`
  or `no` is assigned in the corresponding position of the result.

- yes:

  ([`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), or
  castable to one) corresponding values assigned for entries in `test`
  that are `TRUE`.

- no:

  ([`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md), or
  castable to one) corresponding values assigned for entries in `test`
  that are `FALSE`.

## Value

An [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) with
the common type of `yes` and `no` (as determined by
[`vctrs::vec_cast_common()`](https://vctrs.r-lib.org/reference/vec_cast.html))
and a shape determined by broadcasting `test`, `yes`, and `no` to a
common shape (see the section on broadcasting rules in
[`vignette("rvar")`](https://mc-stan.org/posterior/dev/articles/rvar.md)).
For every element of `draws_of(test)`, the corresponding element of
`draws_of(yes)` or `draws_of(no)` is placed into the result, depending
on whether the element of `test` is `TRUE` or `FALSE`.

## Examples

``` r
x <- rvar(1:4)
y <- rvar(5:8)

i <- rvar(c(TRUE,FALSE,TRUE,FALSE))
z <- rvar_ifelse(i, x, y)
z
#> rvar<4>[1] mean ± sd:
#> [1] 4.5 ± 3.1 
draws_of(z)
#>   [,1]
#> 1    1
#> 2    6
#> 3    3
#> 4    8
```

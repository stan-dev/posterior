# Transform to `draws` objects

Try to transform an R object to a format supported by the posterior
package.

## Usage

``` r
as_draws(x, ...)

is_draws(x)
```

## Arguments

- x:

  (draws) A `draws` object or another R object for which the method is
  defined.

- ...:

  Arguments passed to individual methods (if applicable).

## Value

If possible, a `draws` object in the closest supported format to `x`.
The formats are linked to in the **See Also** section below.

## Details

The class `"draws"` is the parent class of all supported formats, which
also have their own subclasses of the form `"draws_{format}"` (e.g.
`"draws_array"`).

## See also

Other formats:
[`draws_array()`](https://mc-stan.org/posterior/dev/reference/draws_array.md),
[`draws_df()`](https://mc-stan.org/posterior/dev/reference/draws_df.md),
[`draws_list()`](https://mc-stan.org/posterior/dev/reference/draws_list.md),
[`draws_matrix()`](https://mc-stan.org/posterior/dev/reference/draws_matrix.md),
[`draws_rvars()`](https://mc-stan.org/posterior/dev/reference/draws_rvars.md)

## Examples

``` r
# create some random draws
x <- matrix(rnorm(30), nrow = 10)
colnames(x) <- c("a", "b", "c")
str(x)
#>  num [1:10, 1:3] -0.195 1.916 0.702 0.971 -0.128 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:3] "a" "b" "c"

# transform to a draws object
y <- as_draws(x)
str(y)
#>  'draws_matrix' num [1:10, 1:3] -0.195 1.916 0.702 0.971 -0.128 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:10] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:3] "a" "b" "c"
#>  - attr(*, "nchains")= int 1

# remove the draws classes from the object
class(y) <- class(y)[-(1:2)]
str(y)
#>  num [1:10, 1:3] -0.195 1.916 0.702 0.971 -0.128 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:10] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:3] "a" "b" "c"
#>  - attr(*, "nchains")= int 1
```

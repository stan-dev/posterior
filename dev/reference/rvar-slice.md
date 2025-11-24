# Random variable slicing

Operations for slicing
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s and
replacing parts of
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

## Usage

``` r
# S3 method for class 'rvar'
x[[i, ...]]

# S3 method for class 'rvar'
x[[i, ...]] <- value

# S3 method for class 'rvar'
x[..., drop = FALSE]

# S3 method for class 'rvar'
x[i, ...] <- value
```

## Arguments

- x:

  an [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

- i, ...:

  indices; see *Details*.

- value:

  (`rvar` or coercable to `rvar`) Value to insert into `x` at the
  location determined by the indices.

- drop:

  (logical) Should singular dimensions be dropped when slicing array
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s? Unlike
  base array slicing operations, defaults to `FALSE`.

## Details

The [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)
slicing operators (`[` and `[[`) attempt to implement the same semantics
as the [base array slicing
operators](https://rdrr.io/r/base/Extract.html). There are some
exceptions; most notably,
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) slicing
defaults to `drop = FALSE` instead of `drop = TRUE`.

## Extracting or replacing single elements with `[[`

The `[[` operator extracts (or replaces) single elements. It always
returns (or replaces) a scalar (length-1)
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

The `x[[i,...]]` operator can be used as follows:

- `x[[<numeric>]]` for scalar numeric `i`: gives the `i`th element of
  `x`. If `x` is multidimensional (i.e. `length(dim(x)) > 1`), extra
  dimensions are ignored when indexing. For example, if `x` is a \\6
  \times 2\\
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) array,
  the 7th element, `x[[7]]`, will be the first element of the second
  column, `x[1,2]`.

- `x[[<numeric rvar>]]` for scalar numeric
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) `i`: a
  generalization of indexing when `i` is a scalar numeric. Within each
  draw of `x`, selects the element corresponding to the value of `i`
  within that same draw.

- `x[[<character>]]` for scalar character `i`: gives the element of `x`
  with name equal to `i`. **Unlike with base arrays**, does not work
  with multidimensional
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md)s.

- `x[[i_1,i_2,...,i_n]]` for scalar numeric or character `i_1`, `i_2`,
  etc. Must provide exactly the same number of indices as dimensions in
  `x`. Selects the element at the corresponding position in the
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) by
  number and/or dimname (as a string).

## Extracting or replacing multiple elements with `[`

The `[` operator extracts (or replaces) multiple elements. It always
returns (or replaces) a possibly-multidimensional
[`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md).

The `x[i,...]` operator can be used as follows:

- `x[<logical>]` for vector logical `i`: `i` is recycled to the same
  length as `x`, ignoring multiple dimensions in `x`, then an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) vector
  is returned containing the elements in `x` where `i` is `TRUE`.

- `x[<logical rvar>]` for scalar logical
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) `i`:
  returns an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) the same
  shape as `x` containing only those draws where `i` is `TRUE`.

- `x[<numeric>]` for vector numeric `i`: an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) vector
  is returned containing the `i`th elements of `x`, ignoring dimensions.

- `x[<matrix>]` for numeric matrix `i`, where
  `ncol(i) == length(dim(x))`: each row of `i` should give the
  multidimensional index for a single element in `x`. The result is an
  [`rvar`](https://mc-stan.org/posterior/dev/reference/rvar.md) vector
  of length `nrow(i)` containing elements of `x` selected by each row of
  `i`.

- `x[i_1,i_2,...,i_n]` for vector numeric, character, or logical `i_1`,
  `i_2`, etc. Returns a slice of `x` containing all elements from the
  dimensions specified in `i_1`, `i_2`, etc. If an argument is left
  empty, all elements from that dimension are included. Unlike base
  arrays, trailing dimensions can be omitted entirely and will still be
  selected; for example, if `x` has three dimensions, both `x[1,,]` and
  `x[1,]` can be used to create a slice that includes all elements from
  the last two dimensions. Unlike base arrays, `[` defaults to
  `drop = FALSE`, so results retain the same number of dimensions as
  `x`.

## Examples

``` r
x <- rvar(array(1:24, dim = c(4,2,3)))
dimnames(x) <- list(c("a","b"), c("d","e","f"))
x
#> rvar<4>[2,3] mean ± sd:
#>   d           e           f          
#> a  2.5 ± 1.3  10.5 ± 1.3  18.5 ± 1.3 
#> b  6.5 ± 1.3  14.5 ± 1.3  22.5 ± 1.3 

## Slicing single elements
# x[[<numeric>]]
x[[2]]
#> rvar<4>[1] mean ± sd:
#> [1] 6.5 ± 1.3 

# x[[<numeric rvar>]]
# notice the draws of x[1:4]...
draws_of(x[1:4])
#>   [,1] [,2] [,3] [,4]
#> 1    1    5    9   13
#> 2    2    6   10   14
#> 3    3    7   11   15
#> 4    4    8   12   16
x[[rvar(c(1,3,4,4))]]
#> rvar<4>[1] mean ± sd:
#> [1] 10 ± 6.9 
# ... x[[rvar(c(1,3,4,4))]] creates a mixures of those draws
draws_of(x[[rvar(c(1,3,4,4))]])
#>   [,1]
#> 1    1
#> 2   10
#> 3   15
#> 4   16

# x[[i_1,i_2,...]]
x[[2,"e"]]
#> rvar<4>[1] mean ± sd:
#> [1] 14 ± 1.3 


## Slicing multiple elements
# x[<logical>]
x[c(TRUE,TRUE,FALSE)]
#> rvar<4>[4] mean ± sd:
#> [1]  2.5 ± 1.3   6.5 ± 1.3  14.5 ± 1.3  18.5 ± 1.3 

# x[<logical rvar>]
# select every other draw
x[rvar(c(TRUE,FALSE,TRUE,FALSE))]
#> rvar<2>[2,3] mean ± sd:
#>   d         e         f        
#> a  2 ± 1.4  10 ± 1.4  18 ± 1.4 
#> b  6 ± 1.4  14 ± 1.4  22 ± 1.4 

# x[<numeric>]
x[1:3]
#> rvar<4>[3] mean ± sd:
#> [1]  2.5 ± 1.3   6.5 ± 1.3  10.5 ± 1.3 

# x[<matrix>]
x[rbind(
  c(1,2),
  c(1,3),
  c(2,2)
)]
#> rvar<4>[3] mean ± sd:
#> [1] 10 ± 1.3  18 ± 1.3  14 ± 1.3 

# x[i_1,i_2,...,i_n]
x[1,]
#> rvar<4>[1,3] mean ± sd:
#>   d           e           f          
#> a  2.5 ± 1.3  10.5 ± 1.3  18.5 ± 1.3 
x[1,2:3]
#> rvar<4>[1,2] mean ± sd:
#>   e         f        
#> a 10 ± 1.3  18 ± 1.3 
x[,2:3]
#> rvar<4>[2,2] mean ± sd:
#>   e         f        
#> a 10 ± 1.3  18 ± 1.3 
#> b 14 ± 1.3  22 ± 1.3 
```

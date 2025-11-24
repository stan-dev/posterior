# Extract parts of a `draws_matrix` object

Extract parts of a `draws_matrix` object. They are strictly defined as
matrices (draws x variable) so dropping any of the dimensions breaks the
expected structure of the object. Accordingly, no dropping of dimensions
is done by default even if the extracted slices are of length 1. If
`drop` is manually set to `TRUE` and any of the dimensions is actually
dropped, this will lead to dropping the `"draws_matrix"` class as well.

## Usage

``` r
# S3 method for class 'draws_matrix'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x, i, j, ..., drop:

  Same as in the default extraction method but with `drop` being set to
  `FALSE` by default.

## Value

An object of class `"draws_matrix"` unless any of the dimensions was
dropped during the extraction.

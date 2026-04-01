# Extract parts of a `draws_array` object

Extract parts of a `draws_array` object. They are strictly defined as
arrays of 3 dimensions (iteration x chain x variable) so dropping any of
the dimensions breaks the expected structure of the object. Accordingly,
no dropping of dimensions is done by default even if the extracted
slices are of length 1. If `drop` is manually set to `TRUE` and any of
the dimensions is actually dropped, this will lead to dropping the
`"draws_array"` class as well.

## Usage

``` r
# S3 method for class 'draws_array'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x, i, j, ..., drop:

  Same as in the default extraction method but with `drop` being set to
  `FALSE` by default.

## Value

An object of class `"draws_array"` unless any of the dimensions was
dropped during the extraction.

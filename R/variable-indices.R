# helpers for manipulating array indices embedded in variable names


# flattening dimensions into variable names with indices -----------------------

#' Flatten the indices of an array-like object.
#' @param x an array or array-like object (e.g. an rvar)
#' @param base_name a base name which, if provided, is prepended to the
#'   resulting index names.
#' @returns a character vector of the same length as `x`, giving index names
#'   names of the form `"base_name[i,j,k,...]"` where `i`, `j`, `k`, ... are indices
#'   (either numeric indices or strings if x has `dimnames`) that correspond to
#'   the indices of that element in the input `x`.
#' @noRd
flatten_indices <- function(x, base_name = "") {
  # determine new dimension names in the form x,y,z
  # start with numeric names
  .length <- length(x)
  .dim <- dim(x) %||% .length
  .dimnames <- dimnames(x)
  dimname_lists <- lapply(.dim, seq_len)
  if (!is.null(.dimnames)) {
    # where character names are provided, use those instead of the numeric names
    dimname_lists = lapply(seq_along(dimname_lists), function(i) .dimnames[[i]] %||% dimname_lists[[i]])
  }
  # expand out the dimname lists into the appropriate combinations and assemble into new names
  dimname_grid <- expand.grid(dimname_lists, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  new_names <- do.call(paste, c(list(sep = ","), dimname_grid))

  # prepend the base_name
  if (isTRUE(.dim == 1)) {
    # scalar, just use the base name
    new_names <- base_name
  } else if (.length >= 1) {
    # non-zero length, put the indices in brackets
    new_names <- paste0(base_name, "[", new_names, "]")
  }

  new_names
}

#' Flatten an array so that it is one-dimensional, setting the name of each
#' element of the output array to match its indices in the input array.
#' @param x an array or array-like object (e.g. an rvar)
#' @param base_name a base name which, if provided, is prepended to the
#'   resulting dimension names.
#' @returns a modified array-like object with `dim(out) == length(x)` and, so
#'   long as the input is not scalar, names of the form `"i,j,k,..."` where
#'   `i`, `j`, `k`, ... are indices from the input `x`. If `base_name` is provided,
#'   names of `out` will have the form `"base_name[i,j,k,...]"`.
#' @noRd
flatten_array <- function(x, base_name = "") {
  new_names <- flatten_indices(x, base_name)
  dim(x) <- length(x)
  names(x) <- new_names
  x
}

#' Fast conversion of rvar draws to a flattened data frame. Equivalent to
#' as.data.frame(draws_of(flatten_array(x, name))) except it works with
#' factor rvars (as.data.frame does not work on array-like factors)
#' @noRd
flatten_rvar_draws_to_df <- function(x, base_name = "") {
  if (length(x) == 0) {
    out <- data.frame(row.names = draw_ids(x))
  } else {
    draws <- draws_of(flatten_array(x, base_name))
    cols <- lapply(seq_len(ncol(draws)), function(i) unname(draws[, i]))
    names(cols) <- colnames(draws)
    out <- vctrs::new_data_frame(cols)
  }
  out
}


# splitting flattened variable names into base names and indices ---------------

#' Given a vector of variable names possibly containing indices, split them into
#' a structure representing the base name and the indices
#' @param x a character vector of names, possibly containing indices;
#' e.g. `c("x", "y[1,2]", NA)`
#' @returns a data frame with `length(x)` rows and these columns:
#'  - `base_name`: character vector of base names;
#'     e.g. `c("x", "y", NA)`
#'  - `indices`: character vector of the index strings if present;
#'     e.g. `c("", "[1,2]", NA)`
#' If an input element of `x` is `NA`, the corresponding entries in the returned
#' data frame will also be `NA`.
#' @noRd
split_variable_names <- function(x) {
  # split x[y,z] names into base name and indices
  # we do this with a regex matching just the indices at the end instead of a
  # single regex matching the entire string with multiple capture groups
  # --- something like regexec("^(.*?)(\\[(.*)\\])?$", x) --- because
  # regexec() + regmatches() is slow on a very large number of variables
  # (because it uses lists), but regexpr() + substr() is vectorized
  indices_matches <- regexpr("(\\[.*\\])?$", x)

  vctrs::new_data_frame(list(
    base_name = substr(x, 1L, indices_matches - 1L),
    indices = substr(x, indices_matches, .Machine$integer.max)
  ))
}

#' Given a vector of index strings (such as returned by
#' `split_variable_names(x)$indices`), split each index string into a character
#' vector of indices.
#' @param x a character vector of index strings;
#' e.g. `c("", "[1,2]", NA)`
#' @returns a list of character vectors of indices;
#' e.g. list(character(), c("1", "2"), NA)
#' @noRd
split_indices <- function(x) {
  strsplit(substr(x, 2, nchar(x) - 1), ",", fixed = TRUE)
}


# manipulating flattened variable names -----------------------------------

#' Given a vector of names possibly containing indices, return the base names
#' @param x a character vector of names, possibly containing indices (e.g. `"x[1,2]"`)
#' @returns a character vector of all unique base names in `x`
#' @noRd
base_names <- function(x) {
  unique(split_variable_names(x)$base_name)
}

#' Given a vector of names possibly containing indices, modify the base names
#' @param x a character vector of names, possibly containing indices (e.g. `"x[1,2]"`)
#' @param value a character vector of replacement base names of `length(base_names(x))`.
#' @returns a character vector of `length(x)` with the base names in the original
#'   strings replaced with `value`.
#' @noRd
`base_names<-` <- function(x, value) {
  vars <- split_variable_names(x)
  base_names <- unique(vars$base_name)
  if (length(value) != length(base_names)) {
    stop_no_call(
      "Attempting to replace variables with ", length(base_names), " unique base names",
      " with ", length(value), " new base names. Lengths must match.\n",
      " - Existing base names: ", toString(base_names, width = 40), "\n",
      " - Replacements:        ", toString(value, width = 40)
    )
  }
  base_name_i <- match(vars$base_name, base_names)
  paste0(value[base_name_i], vars$indices)
}

#' Given a vector of variable names, either return the variable names or the
#' base names of the variables, depending on the value of `with_indices`
#' @noRd
variable_names <- function(x, with_indices = TRUE) {
  with_indices <- as_one_logical(with_indices)
  if (with_indices) {
    x
  } else {
    base_names(x)
  }
}

#' Given a vector of variable names, either replace the variable names or the
#' base names of the variables, depending on the value of `with_indices`
#' @noRd
`variable_names<-` <- function(x, with_indices = TRUE, value) {
  with_indices <- as_one_logical(with_indices)
  if (with_indices) {
    value
  } else {
    base_names(x) <- value
    x
  }
}

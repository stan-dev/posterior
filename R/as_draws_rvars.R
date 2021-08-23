#' The `draws_rvars` format
#'
#' @name draws_rvars
#' @family formats
#'
#' @templateVar draws_format draws_rvars
#' @templateVar base_class "list"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_rvars"` are lists of [`rvar`] objects.
#' See **Examples**.
#'
NULL

#' @rdname draws_rvars
#' @export
as_draws_rvars <- function(x, ...) {
  UseMethod("as_draws_rvars")
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.default <- function(x, ...) {
  x <- as_draws(x)
  as_draws_rvars(x, ...)
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.draws_rvars <- function(x, ...) {
  x
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.list <- function(x, ...) {
  if (all(vapply(x, is_rvar, logical(1)))) {
    .as_draws_rvars(x, ...)
  } else {
    NextMethod()
  }
}

#' @export
as_draws_rvars.rvar <- function(x, ...) {
  .as_draws_rvars(list(x = x), ...)
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.draws_matrix <- function(x, ...) {
  if (ndraws(x) == 0) {
    return(empty_draws_rvars(variables(x)))
  }

  # split x[y,z] names into base name and indices
  vars_indices <- strsplit(variables(x), "(\\[|\\])")
  vars <- sapply(vars_indices, `[[`, 1)

  # pull out each var into its own rvar
  var_names <- unique(vars)
  rvars_list <- lapply(var_names, function (var) {
    var_i <- vars == var
    # reset class here as otherwise the draws arrays in the output rvars
    # have type draws_matrix, which makes inspecting them hard
    var_matrix <- unclass(x[, var_i, drop = FALSE])
    attr(var_matrix, "nchains") <- NULL

    if (ncol(var_matrix) == 1) {
      # single variable, no indices
      out <- rvar(var_matrix)
      dimnames(out) <- NULL
    } else {
      # variable with indices => we need to reshape the array
      # basically, we're going to do a bunch of work up front to figure out
      # a single array slice that does most of the work for us.

      # first, pull out the list of indices into a data frame
      # where each column is an index variable
      indices <- sapply(vars_indices[var_i], `[[`, 2)
      indices <- as.data.frame(do.call(rbind, strsplit(indices, ",")),
                               stringsAsFactors = FALSE)
      unique_indices <- vector("list", length(indices))
      .dimnames <- vector("list", length(indices))
      names(unique_indices) <- names(indices)
      for (i in seq_along(indices)) {
        numeric_index <- suppressWarnings(as.numeric(indices[[i]]))
        if (!anyNA(numeric_index) && rlang::is_integerish(numeric_index)) {
          # for integer indices, we need to convert them to integers
          # so that we can sort them in numerical order (not string order)
          if (min(numeric_index) >= 1) {
            # integer indices >= 1 are forced to lower bound of 1 + no dimnames
            indices[[i]] <- as.integer(numeric_index)
            unique_indices[[i]] <- seq.int(1, max(numeric_index))
          } else {
            # indices with values < 1 are sorted but otherwise left as-is, and will create dimnames
            indices[[i]] <- numeric_index
            unique_indices[[i]] <- sort(unique(numeric_index))
            .dimnames[[i]] <- unique_indices[[i]]
          }
        } else {
          # we convert non-numeric indices to factors so that we can force them
          # to be ordered as they appear in the data (rather than in alphabetical order)
          factor_levels <- unique(indices[[i]])
          indices[[i]] <- factor(indices[[i]], levels = factor_levels)
          # these aren't sorted so they appear in original order
          unique_indices[[i]] <- factor(factor_levels, levels = factor_levels)
          .dimnames[[i]] <- unique_indices[[i]]
        }
      }

      # sort indices and fill in missing indices as NA to ensure
      # (1) even if the order of the variables is something weird (like the
      # column for x[2,2] comes before x[1,1] in the matrix) the result
      # places those columns in the correct cells of the array
      # (2) if some combination of indices is missing (say x[2,1] isn't
      # in the input) that cell in the array gets an NA

      # Use expand.grid to get all cells in output array. We reverse indices
      # here because it helps us do the sort after the merge, where
      # we need to sort in reverse order of the indices (because
      # the value of the last index should move slowest)
      all_indices <- expand.grid(rev(unique_indices))
      # merge with all.x = TRUE (left join) to fill in missing cells with NA
      indices <- merge(all_indices, cbind(indices, index = seq_len(nrow(indices))),
                       all.x = TRUE, sort = FALSE)
      # need to do the sort manually after merge because when sort = TRUE, merge
      # sorts factors as if they were strings, and we need factors to be sorted as factors
      indices <- indices[do.call(order, as.list(indices[, -ncol(indices), drop = FALSE])),]

      # re-sort the array and fill in missing cells with NA
      var_matrix <- var_matrix[, indices$index, drop = FALSE]

      # convert to rvar and adjust dimensions
      out <- rvar(var_matrix)
      dim(out) <- unname(lengths(unique_indices))
      dimnames(out) <- .dimnames
    }
    out
  })
  names(rvars_list) <- var_names
  out <- .as_draws_rvars(rvars_list, ...)
  .nchains <- nchains(x)
  for (i in seq_along(out)) {
    nchains_rvar(out[[i]]) <- .nchains
  }
  out
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.draws_array <- function(x, ...) {
  as_draws_rvars(as_draws_matrix(x), ...)
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.draws_df <- as_draws_rvars.draws_array

#' @rdname draws_rvars
#' @export
as_draws_rvars.draws_list <- as_draws_rvars.draws_array

#' @rdname draws_rvars
#' @export
as_draws_rvars.mcmc <- function(x, ...) {
  as_draws_rvars(as_draws_matrix(x), ...)
}

#' @rdname draws_rvars
#' @export
as_draws_rvars.mcmc.list <- function(x, ...) {
  as_draws_rvars(as_draws_array(x), ...)
}

# try to convert any R object into a 'draws_rvars' object
.as_draws_rvars <- function(x, ...) {
  x <- as.list(x)

  # convert all elements to rvars
  x <- lapply(x, as_rvar)

  # replace blank variable names with defaults
  if (is.null(names(x))) {
    names(x) <- default_variables(length(x))
  } else {
    blank_names <- nchar(names(x)) == 0
    names(x)[blank_names] <- default_variables(length(x))[blank_names]
  }

  check_new_variables(names(x))

  x <- conform_rvar_ndraws_nchains(x)

  class(x) <- class_draws_rvars()
  x
}

#' @rdname draws_rvars
#' @export
draws_rvars <- function(..., .nchains = 1) {
  out <- lapply(list(...), function(x) {
    if (is_rvar(x)) x
    else rvar(x, nchains = .nchains)
  })

  if (!rlang::is_named(out)) {
    stop_no_call("All variables must be named.")
  }

  .as_draws_rvars(out)
}

class_draws_rvars <- function() {
  c("draws_rvars", "draws", "list")
}

#' @rdname draws_rvars
#' @export
is_draws_rvars <- function(x) {
  inherits(x, "draws_rvars")
}

# is an object looking like a 'draws_rvars' object?
is_draws_rvars_like <- function(x) {
  is.list(x) && all(sapply(x, is_rvar))
}

#' @export
`[.draws_rvars` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}

# create an empty draws_rvars object
empty_draws_rvars <- function(variables = character(0), nchains = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  out <- named_list(variables, rvar())
  class(out) <- class_draws_rvars()
  out
}

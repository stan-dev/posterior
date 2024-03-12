#' Get variable names from `draws` objects
#'
#' Get variable names from [`draws`] objects.
#'
#' @name variables
#' @template args-methods-x
#' @template args-methods-dots
#' @template args-methods-reserved
#' @template args-methods-with_indices
#'
#' @details
#' `variables()` returns a vector of all variable names, and `nvariables()`
#' returns the number of variables.
#'
#' @return
#'
#' For `variables()`, a character vector.
#'
#' For `nvariables()`, a scalar integer.
#'
#' @seealso [`variables<-`], [`rename_variables`], [`draws-index`]
#'
#' @examples
#' x <- example_draws()
#'
#' variables(x)
#' nvariables(x)
#' variables(x) <- letters[1:nvariables(x)]
#' @export
variables <- function(x, ...) {
  UseMethod("variables")
}

#' @export
variables.NULL <- function(x, ...) {
  NULL
}

#' @rdname variables
#' @export
variables.draws_matrix <- function(x, reserved = FALSE, with_indices = TRUE, ...) {
  out <- remove_reserved_variable_names(colnames(x), reserved)
  variable_names(out, with_indices)
}

#' @rdname variables
#' @export
variables.draws_array <- function(x, reserved = FALSE, with_indices = TRUE, ...) {
  out <- remove_reserved_variable_names(dimnames(x)[[3L]], reserved)
  variable_names(out, with_indices)
}

#' @rdname variables
#' @export
variables.draws_df <- function(x, reserved = FALSE, with_indices = TRUE, ...) {
  # reserved_df_variables are special data.frame columns
  # which should never be included as variables
  out <- names(x)[!names(x) %in% reserved_df_variables()]
  out <- remove_reserved_variable_names(out, reserved)
  variable_names(out, with_indices)
}

#' @rdname variables
#' @export
variables.draws_list <- function(x, reserved = FALSE, with_indices = TRUE, ...) {
  if (!length(x)) {
    return(character(0))
  }
  out <- remove_reserved_variable_names(names(x[[1]]), reserved)
  variable_names(out, with_indices)
}

#' @rdname variables
#' @export
variables.draws_rvars <- function(x, reserved = FALSE, with_indices = FALSE, ...) {
  with_indices <- as_one_logical(with_indices)
  if (with_indices) {
    out <- unlist(.mapply(flatten_indices, list(x, names(x)), NULL), recursive = FALSE, use.names = FALSE)
  } else {
    out <- names(x)
  }
  remove_reserved_variable_names(out, reserved)
}

# remove reserved variable names
remove_reserved_variable_names <- function(variables, reserved) {
  reserved <- as_one_logical(reserved)
  if (!reserved && length(variables)) {
    # can't use setdiff() here as in the edge case where someone
    # manually creates duplicate columns it will give incorrect results
    variables <- variables[!variables %in% reserved_variables()]
  }
  variables
}


#' Set variable names in `draws` objects
#'
#' Set variable names for all variables in a [`draws`] object. The
#' `set_variables()` form is useful when using pipe operators.
#'
#' @template args-methods-x
#' @template args-methods-dots
#' @template args-methods-with_indices
#' @param value,variables (character vector) new variable names.
#'
#' @details
#' `variables(x) <- value` allows you to modify the vector of variable names,
#' similar to how `names(x) <- value` works for vectors and lists. For renaming
#' specific variables, `set_variables(x, value)` works equivalently, but is more intuitive
#' when using the pipe operator.
#'
#' For renaming specific variables, [rename_variables()] may offer a more
#' convenient approach.
#'
#' @return Returns a [`draws`] object of the same format as `x`, with
#'   variables named as specified.
#'
#' @seealso [`variables`], [`rename_variables`], [`draws-index`]
#'
#' @examples
#' x <- example_draws()
#'
#' variables(x)
#' nvariables(x)
#' variables(x) <- letters[1:nvariables(x)]
#'
#' # or equivalently...
#' x <- set_variables(x, letters[1:nvariables(x)])
#'
#' @export
`variables<-` <- function(x, ..., value) {
  UseMethod("variables<-")
}

#' @rdname variables-set
#' @export
`variables<-.draws_matrix` <- function(x, with_indices = TRUE, ..., value) {
  check_new_variables(value)
  variable_names(colnames(x), with_indices) <- value
  x
}

#' @rdname variables-set
#' @export
`variables<-.draws_array` <- function(x, with_indices = TRUE, ..., value) {
  check_new_variables(value)
  variable_names(dimnames(x)[[3L]], with_indices) <- value
  x
}

#' @rdname variables-set
#' @export
`variables<-.draws_df` <- function(x, with_indices = TRUE, ..., value) {
  check_new_variables(value)
  names_i <- !names(x) %in% reserved_df_variables()
  variable_names(names(x)[names_i], with_indices) <- value
  x
}

#' @rdname variables-set
#' @export
`variables<-.draws_list` <- function(x, with_indices = TRUE, ..., value) {
  check_new_variables(value)
  for (i in seq_along(x)) {
    variable_names(names(x[[i]]), with_indices) <- value
  }
  x
}

#' @rdname variables-set
#' @export
`variables<-.draws_rvars` <- function(x, with_indices = FALSE, ..., value) {
  with_indices <- as_one_logical(with_indices)
  check_new_variables(value)
  if (with_indices) {
    # need to make sure that the provided names only change the base names of
    # the variables and that the indexes otherwise match
    vars <- split_variable_names(value)
    base_names <- unique(vars$base_name)
    base_name_i <- match(vars$base_name, base_names)

    x_indices <- ulapply(x, flatten_indices, recursive = FALSE, use.names = FALSE)
    x_base_name_i <- rep(seq_along(x), lengths(x))
    if (!identical(x_base_name_i, base_name_i) || !identical(x_indices, vars$indices)) {
      stop_no_call(
        "variables(<draws_rvars>) <- value is only allowed when the indices in `value` match ",
        "the indices in the original names. To modify the names of the indices, either modify ",
        "the dims of the underlying rvars or convert to another draws format first."
      )
    }

    names(x) <- base_names
  } else {
    names(x) <- value
  }
  x
}

#' @rdname variables-set
#' @export
set_variables <- function(x, variables, ...) {
  variables(x, ...) <- variables
  x
}

#' @rdname variables
#' @export
nvariables <- function(x, ...) {
  UseMethod("nvariables")
}
#' @export
nvariables.NULL <- function(x, ...) {
  0
}

#' @export
nvariables.draws <- function(x, ...) {
  length(variables(x, ...))
}


# internal ----------------------------------------------------------------

# check validity of existing variable names: e.g., that
# all `variables` exist in `x` and that no `variables`are reserved words
# Additionally, this returns the canonical name, so e.g. "theta" will get
# converted to c("theta[1]", "theta[2]", ...) if those variables exist.
# @param regex should 'variables' be treated as regular expressions?
# @param scalar should only scalar variables be matched?
check_existing_variables <- function(variables, x, regex = FALSE,
                                     scalar = FALSE, exclude = FALSE) {
  check_draws_object(x)
  if (is.null(variables)) {
    return(NULL)
  }

  regex <- as_one_logical(regex)
  scalar <- as_one_logical(scalar)
  exclude <- as_one_logical(exclude)
  variables <- unique(as.character(variables))
  all_variables <- variables(x, reserved = TRUE)

  if (regex) {
    tmp <- named_list(variables)
    for (i in seq_along(variables)) {
      tmp[[i]] <- grep(variables[i], all_variables)
    }
    # regular expressions are not required to match anything
    missing_variables <- NULL
    variables <- as.character(all_variables[unique(unlist(tmp))])
  } else if (!scalar) {
    # need to find variables that are matched by either a scalar or vector
    # variable in x and what the matching variable is, while keeping original
    # order of input `variables`

    # find scalar variables (1-to-1 match between all_variables and variables)
    scalar_input_ixs <- match(all_variables, variables)
    # find vector variable matches (match all_variables with the indexing stripped)
    all_variables_base <- all_variables
    # exclude already matched scalar variables
    all_variables_base[!is.na(scalar_input_ixs)] <- NA_character_
    all_variables_base <- split_variable_names(all_variables_base)$base_name
    vector_input_ixs <- match(all_variables_base, variables)
    # compose the vector of indices of matched input variables
    input_ixs <- c(scalar_input_ixs[!is.na(scalar_input_ixs)],
                   vector_input_ixs[!is.na(vector_input_ixs)])
    # compose the vector of indices of matched all_variables
    all_var_ixs <- seq_along(all_variables)
    all_var_matched_ixs <- c(all_var_ixs[!is.na(scalar_input_ixs)],
                             all_var_ixs[!is.na(vector_input_ixs)])
    # select missed input variables
    missing_vars_mask <- rep_len(TRUE, length(variables))
    missing_vars_mask[input_ixs] <- FALSE
    missing_variables <- variables[missing_vars_mask]
    # select matched all_variables maintaining the input variables order
    variables <- all_variables[all_var_matched_ixs[order(input_ixs, all_var_matched_ixs)]]
  } else {
    missing_variables <- setdiff(variables, all_variables)
  }
  variables <- check_reserved_variables(variables)
  if (length(missing_variables)) {
    stop_no_call("The following variables are missing in the draws object: ",
          comma(missing_variables))
  }

  # handle excluding variables for subset_draws
  if (exclude) {
    variables <- setdiff(all_variables, variables)
  }

  invisible(variables)
}

# check validity of new variables: e.g., that there are
# no duplicates in `variables` and that they do not use
# reserved words
check_new_variables <- function(variables) {
  # use anyDuplicated() for the check since it is faster than any(duplicated(x)) and
  # we shouldn't expect to take this branch often (since it is an error)
  if (anyDuplicated(variables)) {
    duplicates = unique(variables[duplicated(variables)])
    stop_no_call(
      "Duplicate variable names are not allowed in draws objects.\n",
      "The following variable names are duplicates:\n",
      comma(duplicates)
    )
  }
  check_reserved_variables(variables)
}

# check variables do not make use of reserved words
check_reserved_variables <- function(variables) {
  assert_character(variables)
  # for now only check reserved columns used in 'draws_df' objects
  # other reserved variables such as '.log_weight' may be overwritten
  # this has the advantage that power users can directly add such variables
  used_reserved_variables <- intersect(reserved_df_variables(), variables)
  if (length(used_reserved_variables)) {
    stop_no_call("Variable names ", comma(used_reserved_variables), " are reserved.")
  }
  invisible(variables)
}

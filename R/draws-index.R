#' Index `draws` objects
#'
#' Index variables, iterations, chains, and draws.
#'
#' @name draws-index
#' @template args-methods-x
#' @template args-methods-dots
#' @param value (character vector) For `variables(x) <- value`, the new variable
#'   names to use.
#'
#' @details
#' The methods `variables()`, `iteration_ids()`, `chain_ids()`, and `draw_ids()` return
#' vectors of all variables, iterations, chains, and draws, respectively. In
#' contrast, the methods `nvariables()`, `niterations()`, `nchains()`, and
#' `ndraws()` return the number of variables, iterations, chains, and draws,
#' respectively.
#'
#' `variables(x) <- value` allows you to modify the vector of variable names,
#' similar to how `names(x) <- value` works for vectors and lists. For renaming
#' specific variables, [rename_variables()] may offer a more convenient approach.
#'
#' @return
#'
#' For `variables()`, a character vector.
#'
#' For `iteration_ids()`, `chain_ids()`, and `draw_ids()`, an integer vector.
#'
#' For `niterations()`, `nchains()`, and `ndraws()`, a scalar integer.
#'
#' @examples
#' x <- example_draws()
#'
#' variables(x)
#' nvariables(x)
#' variables(x) <- letters[1:nvariables(x)]
#'
#' iteration_ids(x)
#' niterations(x)
#'
#' chain_ids(x)
#' nchains(x)
#'
#' draw_ids(x)
#' ndraws(x)
#'
NULL

#' @rdname draws-index
#' @export
variables <- function(x, ...) {
  UseMethod("variables")
}

#' @export
variables.NULL <- function(x, ...) {
  NULL
}

#' @export
variables.draws_matrix <- function(x, reserved = FALSE, ...) {
  remove_reserved_variable_names(colnames(x), reserved)
}

#' @export
variables.draws_array <- function(x, reserved = FALSE, ...) {
  remove_reserved_variable_names(dimnames(x)[[3L]], reserved)
}

#' @export
variables.draws_df <- function(x, reserved = FALSE, ...) {
  # reserved_df_variables are special data.frame columns
  # which should never be included as variables
  out <- names(x)[!names(x) %in% reserved_df_variables()]
  remove_reserved_variable_names(out, reserved)
}

#' @export
variables.draws_list <- function(x, reserved = FALSE, ...) {
  if (!length(x)) {
    return(character(0))
  }
  remove_reserved_variable_names(names(x[[1]]), reserved)
}

#' @export
variables.draws_rvars <- function(x, reserved = FALSE, ...) {
  remove_reserved_variable_names(names(x), reserved)
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

#' @rdname draws-index
#' @export
`variables<-` <- function(x, value) {
  UseMethod("variables<-")
}

#' @export
`variables<-.draws_matrix` <- function(x, value) {
  check_new_variables(value)
  colnames(x) <- value
  x
}

#' @export
`variables<-.draws_array` <- function(x, value) {
  check_new_variables(value)
  dimnames(x)[[3L]] <- value
  x
}

#' @export
`variables<-.draws_df` <- function(x, value) {
  check_new_variables(value)
  names(x)[!names(x) %in% reserved_df_variables()] <- value
  x
}

#' @export
`variables<-.draws_list` <- function(x, value) {
  check_new_variables(value)
  for (i in seq_along(x)) {
    names(x[[i]]) <- value
  }
  x
}

#' @export
`variables<-.draws_rvars` <- function(x, value) {
  check_new_variables(value)
  names(x) <- value
  x
}

#' @rdname draws-index
#' @export
iteration_ids <- function(x) {
  UseMethod("iteration_ids")
}

#' @export
iteration_ids.NULL <- function(x) {
  NULL
}

#' @export
iteration_ids.draws_matrix <- function(x) {
  if (nchains(x) > 1) {
    out <- seq_len(niterations(x))
  } else {
    out <- draw_ids(x)
  }
  out
}

#' @export
iteration_ids.draws_array <- function(x) {
  out <- rownames(x) %||% seq_rows(x)
  as.integer(out)
}

#' @export
iteration_ids.draws_df <- function(x) {
  as.integer(unique(x$.iteration))
}

#' @export
iteration_ids.draws_list <- function(x) {
  seq_along(x[[1]][[1]])
}

#' @export
iteration_ids.draws_rvars <- function(x) {
  iteration_ids(x[[1]])
}

#' @export
iteration_ids.rvar <- function(x) {
  if (nchains(x) > 1) {
    out <- seq_len(niterations(x))
  } else {
    out <- draw_ids(x)
  }
  out
}

#' @rdname draws-index
#' @export
chain_ids <- function(x) {
  UseMethod("chain_ids")
}

#' @export
chain_ids.NULL <- function(x) {
  NULL
}

#' @export
chain_ids.draws_matrix <- function(x) {
  seq_len(nchains(x))
}

#' @export
chain_ids.draws_array <- function(x) {
  out <- colnames(x) %||% seq_cols(x)
  as.integer(out)
}

#' @export
chain_ids.draws_df <- function(x) {
  as.integer(unique(x$.chain))
}

#' @export
chain_ids.draws_list <- function(x) {
  out <- names(x) %||% seq_rows(x)
  as.integer(out)
}

#' @export
chain_ids.draws_rvars <- function(x) {
  chain_ids(x[[1]])
}

#' @export
chain_ids.draws_rvars <- function(x) {
  seq_len(nchains(x))
}

#' @rdname draws-index
#' @export
draw_ids <- function(x) {
  UseMethod("draw_ids")
}

#' @export
draw_ids.NULL <- function(x) {
  NULL
}

#' @export
draw_ids.draws_matrix <- function(x) {
  as.integer(rownames(x) %||% seq_rows(x))
}

#' @export
draw_ids.draws_array <- function(x) {
  iteration_ids <- iteration_ids(x)
  niterations <- niterations(x)
  chain_ids <- chain_ids(x)
  ulapply(chain_ids, function(c) niterations * (c - 1L) + iteration_ids)
}

#' @export
draw_ids.draws_df <- function(x) {
  as.integer(unique(x$.draw))
}

#' @export
draw_ids.draws_list <- function(x) {
  iteration_ids <- iteration_ids(x)
  niterations <- niterations(x)
  chain_ids <- chain_ids(x)
  ulapply(chain_ids, function(c) niterations * (c - 1L) + iteration_ids)
}

#' @export
draw_ids.draws_rvars <- function(x) {
  draw_ids(x[[1]])
}

#' @export
draw_ids.rvar <- function(x) {
  draws <- draws_of(x)
  out <- rownames(draws) %||% seq_rows(draws)
  as.integer(out)
}

#' @rdname draws-index
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

#' @rdname draws-index
#' @export
niterations <- function(x) {
  UseMethod("niterations")
}

#' @export
niterations.NULL <- function(x) {
  0
}

#' @export
niterations.draws_matrix <- function(x) {
  NROW(x) / nchains(x)
}

#' @export
niterations.draws_array <- function(x) {
  NROW(x)
}

#' @export
niterations.draws_df <- function(x) {
  length(iteration_ids(x))
}

#' @export
niterations.draws_list <- function(x) {
  if (!length(x) || !length(x[[1]])) {
    return(0)
  }
  length(x[[1]][[1]])
}

#' @export
niterations.draws_rvars <- function(x) {
  if (!length(x)) 0 else niterations(x[[1]])
}

#' @export
niterations.rvar <- function(x) {
  ndraws(x) / nchains(x)
}

#' @rdname draws-index
#' @export
nchains <- function(x) {
  UseMethod("nchains")
}

#' @export
nchains.NULL <- function(x) {
  0
}

#' @export
nchains.draws_matrix <- function(x) {
  attr(x, "nchains") %||% 1L
}

#' @export
nchains.draws_array <- function(x) {
  NCOL(x)
}

#' @export
nchains.draws_df <- function(x) {
  length(chain_ids(x))
}

#' @export
nchains.draws_list <- function(x) {
  length(x)
}

#' @export
nchains.draws_rvars <- function(x) {
  if (!length(x)) 0 else nchains(x[[1]])
}

#' @export
nchains.rvar <- function(x) {
  attr(x, "nchains") %||% 1L
}
# for internal use only currently: if you are setting the nchains
# attribute on an rvar, ALWAYS use this function so that the proxy
# cache is invalidated
`nchains_rvar<-` <- function(x, value) {
  attr(x, "nchains") <- value
  invalidate_rvar_cache(x)
}


#' @rdname draws-index
#' @export
ndraws <- function(x) {
  UseMethod("ndraws")
}

#' @export
ndraws.NULL <- function(x) {
  0
}

#' @export
ndraws.draws_matrix <- function(x) {
  NROW(x)
}

#' @export
ndraws.draws_array <- function(x) {
  niterations(x) * nchains(x)
}

#' @export
ndraws.draws_df <- function(x) {
  NROW(x)
}

#' @export
ndraws.draws_list <- function(x) {
  niterations(x) * nchains(x)
}

#' @export
ndraws.draws_rvars <- function(x) {
  if (!length(x)) 0 else ndraws(x[[1]])
}

#' @export
ndraws.rvar <- function(x) {
  # as.vector() to drop names in case there are index names
  as.vector(NROW(draws_of(x)))
}


# internal ----------------------------------------------------------------

# check validity of existing variable names: e.g., that
# all `variables` exist in `x` and that no `variables`are reserved words
# Additionally, this returns the cannonical name, so e.g. "theta" will get
# converted to c("theta[1]", "theta[2]", ...) if those variables exist.
# @param regex should 'variables' be treated as regular expressions?
# @param scalar_only should only scalar variables be matched?
check_existing_variables <- function(variables, x, regex = FALSE,
                                     scalar_only = FALSE) {
  check_draws_object(x)
  if (is.null(variables)) {
    return(NULL)
  }
  regex <- as_one_logical(regex)
  scalar_only <- as_one_logical(scalar_only)
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
  } else if (!scalar_only) {
    # need to find variables that are matched by either a scalar or vector
    # variable in x and what the matching variable is, while keeping original
    # order of `variables` => we'll do two left joins to fill in scalar and
    # vector variable names, which will also find missing variables (as NAs)

    # because the mapping of scalar variables to variables(x) is one-to-one, can
    # do the first left join using match instead of merge, which is faster
    scalar_variables <- all_variables[match(variables, all_variables)]

    # need to use merge for the second join as vector variables may match
    # multiple variables in `x`
    all_variables_base <- gsub("\\[.*\\]$", "", all_variables)
    variables_df <- merge(
      # left join seems to rearrange input order, so need to keep it
      # and the order of variables in x so we can restore them later
      data.frame(
        variable = variables, input_order = seq_along(variables), scalar = scalar_variables,
        stringsAsFactors = FALSE
      ),
      data.frame(
        variable = all_variables_base, vector = all_variables, variable_order = seq_along(all_variables),
        stringsAsFactors = FALSE
      ),
      by = "variable",
      sort = FALSE,
      all.x = TRUE
    )

    vector_or_scalar <- with(variables_df, ifelse(is.na(vector), scalar, vector))
    missing_variables <- variables_df$variable[is.na(vector_or_scalar)]
    variables <- vector_or_scalar[order(variables_df$input_order, variables_df$variable_order)]
    variables <- variables[!is.na(variables)]
  } else {
    missing_variables <- setdiff(variables, all_variables)
  }
  variables <- check_reserved_variables(variables)
  if (length(missing_variables)) {
    stop_no_call("The following variables are missing in the draws object: ",
          comma(missing_variables))
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

# check validity of iteration indices
# @param unique should the returned IDs be unique?
check_iteration_ids <- function(iteration_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(iteration_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  iteration_ids <- as.integer(iteration_ids)
  if (unique) {
    iteration_ids <- unique(iteration_ids)
  }
  iteration_ids <- sort(iteration_ids)
  if (any(iteration_ids < 1L)) {
    stop_no_call("Iteration indices should be positive.")
  }
  niterations <- niterations(x)
  max_iteration <- SW(max(iteration_ids))
  if (max_iteration > niterations) {
    stop_no_call("Tried to subset iterations up to '", max_iteration, "' ",
          "but the object only has '", niterations, "' iterations.")
  }
  invisible(iteration_ids)
}

# check validity of chain indices
# @param unique should the returned IDs be unique?
check_chain_ids <- function(chain_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(chain_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  chain_ids <- as.integer(chain_ids)
  if (unique) {
    chain_ids <- unique(chain_ids)
  }
  chain_ids <- sort(chain_ids)
  if (any(chain_ids < 1L)) {
    stop_no_call("Chain indices should be positive.")
  }
  nchains <- nchains(x)
  max_chain <- SW(max(chain_ids))
  if (max_chain > nchains) {
    stop_no_call("Tried to subset chains up to '", max_chain, "' ",
          "but the object only has '", nchains, "' chains.")
  }
  invisible(chain_ids)
}

# check validity of draw indices
# @param unique should the returned IDs be unique?
check_draw_ids <- function(draw_ids, x, unique = TRUE) {
  check_draws_object(x)
  if (is.null(draw_ids)) {
    return(NULL)
  }
  unique <- as_one_logical(unique)
  draw_ids <- as.integer(draw_ids)
  if (unique) {
    draw_ids <- unique(draw_ids)
  }
  draw_ids <- sort(draw_ids)
  if (any(draw_ids < 1L)) {
    stop_no_call("Draw indices should be positive.")
  }
  ndraws <- ndraws(x)
  max_draw <- SW(max(draw_ids))
  if (max_draw > ndraws) {
    stop_no_call("Tried to subset draws up to '", max_draw, "' ",
          "but the object only has '", ndraws, "' draws.")
  }
  invisible(draw_ids)
}

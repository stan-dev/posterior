#' Print `draws_matrix` objects
#'
#' Pretty printing for [`draws_matrix`] objects.
#'
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_draws
#' @template args-print-max_variables
#' @template args-methods-reserved
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_matrix(example_draws())
#' print(x)
#'
#' @export
print.draws_matrix <- function(x, digits = 2,
                               max_draws = getOption("posterior.max_draws", 10),
                               max_variables = getOption("posterior.max_variables", 8),
                               reserved = FALSE, ...) {
  max_draws <- as_one_integer(max_draws)
  max_variables <- as_one_integer(max_variables)
  reserved <- as_one_logical(reserved)
  niterations <- niterations(x)
  nchains <- nchains(x)
  ndraws <- ndraws(x)
  nvariables <- nvariables(x)
  header <- paste0(
    "# A draws_matrix: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)
  sel_draws <- seq_len(min(max_draws, ndraws))
  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x
  if (!reserved) {
    y <- remove_reserved_variables(y)
  }
  y <- .subset_draws(
    y, draw = sel_draws, variable = sel_variables,
    reserved = reserved
  )
  class(y) <- "matrix"
  print(y, digits = digits, ...)
  more_iterations <- ndraws -  max_draws
  more_variables <- nvariables - max_variables
  if (more_iterations > 0 || more_variables > 0) {
    comment <- character(0)
    if (more_iterations > 0) {
      comment <- c(comment, paste0(more_iterations, " more draws"))
    }
    if (more_variables > 0) {
      comment <- c(comment, paste0(more_variables, " more variables"))
    }
    comment <- paste0(comment, collapse = ", and ")
    comment <- paste0("# ... with ", comment, "\n")
    cat(comment)
  }
  reserved_variables <- reserved_variables(x)
  if (!reserved && length(reserved_variables)) {
    cat0("# ... hidden reserved variables ", comma(reserved_variables), "\n")
  }
  invisible(x)
}

#' Print `draws_array` objects
#'
#' Pretty printing for [`draws_array`] objects.
#'
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_iterations
#' @template args-print-max_chains
#' @template args-print-max_variables
#' @template args-methods-reserved
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_array(example_draws())
#' print(x)
#'
#' @export
print.draws_array <- function(x, digits = 2,
                              max_iterations = getOption("posterior.max_iterations", 5),
                              max_chains = getOption("posterior.max_chains", 8),
                              max_variables = getOption("posterior.max_variables", 4),
                              reserved = FALSE, ...) {
  max_iterations <- as_one_integer(max_iterations)
  max_chains <- as_one_integer(max_chains)
  max_variables <- as_one_integer(max_variables)
  reserved <- as_one_logical(reserved)
  niterations <- niterations(x)
  nchains <- nchains(x)
  nvariables <- nvariables(x)
  header <- paste0(
    "# A draws_array: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)
  sel_iterations <- seq_len(min(max_iterations, niterations))
  sel_chains <- seq_len(min(max_chains, nchains))
  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x
  if (!reserved) {
    y <- remove_reserved_variables(y)
  }
  y <- .subset_draws(
    y, sel_iterations, sel_chains, sel_variables,
    reserved = reserved
  )
  class(y) <- "array"
  print(y, digits = digits, ...)
  more_iterations <- niterations - max_iterations
  more_chains <- nchains - max_chains
  more_variables <- nvariables - max_variables
  if (more_iterations > 0 || more_chains > 0 || more_variables > 0) {
    comment <- character(0)
    if (more_iterations > 0) {
      comment <- c(comment, paste0(more_iterations, " more iterations"))
    }
    if (more_chains > 0) {
      comment <- c(comment, paste0(more_chains, " more chains"))
    }
    if (more_variables > 0) {
      comment <- c(comment, paste0(more_variables, " more variables"))
    }
    comment <- paste0(comment, collapse = ", and ")
    comment <- paste0("# ... with ", comment, "\n")
    cat(comment)
  }
  reserved_variables <- reserved_variables(x)
  if (!reserved && length(reserved_variables)) {
    cat0("# ... hidden reserved variables ", comma(reserved_variables), "\n")
  }
  invisible(x)
}

#' Print `draws_df` objects
#'
#' Pretty printing for [`draws_df`] objects.
#'
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_draws
#' @template args-print-max_variables
#' @template args-methods-reserved
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_df(example_draws())
#' print(x)
#'
#' @export
print.draws_df <- function(x, digits = 2,
                           max_draws = getOption("posterior.max_draws", 10),
                           max_variables = getOption("posterior.max_variables", 8),
                           reserved = FALSE, ...) {
  max_draws <- as_one_integer(max_draws)
  max_variables <- as_one_integer(max_variables)
  reserved <- as_one_logical(reserved)
  niterations <- niterations(x)
  nchains <- nchains(x)
  ndraws <- ndraws(x)
  nvariables <- nvariables(x, reserved = reserved)
  header <- paste0(
    "# A draws_df: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)
  sel_draws <- seq_len(min(max_draws, ndraws))
  sel_variables <- variables(x, reserved = reserved)
  seq_variables <- seq_len(min(max_variables, nvariables))
  sel_variables <- sel_variables[seq_variables]
  y <- .subset_draws(
    x[sel_draws,], variable = sel_variables,
    reserved = reserved
  )
  if (!reserved) {
    # reserved df variables can only be removed after subsetting
    y <- remove_reserved_df_variables(y)
  }
  class(y) <- "data.frame"
  print(y, digits = digits, ...)
  more_iterations <- ndraws - max_draws
  more_variables <- nvariables - max_variables
  if (more_iterations > 0 || more_variables > 0) {
    comment <- character(0)
    if (more_iterations > 0) {
      comment <- c(comment, paste0(more_iterations, " more draws"))
    }
    if (more_variables > 0) {
      comment <- c(comment, paste0(more_variables, " more variables"))
    }
    comment <- paste0(comment, collapse = ", and ")
    comment <- paste0("# ... with ", comment, "\n")
    cat(comment)
  }
  reserved_variables <- all_reserved_variables(x)
  if (!reserved && length(reserved_variables)) {
    cat0("# ... hidden reserved variables ", comma(reserved_variables), "\n")
  }
  invisible(x)
}

#' Print `draws_list` objects
#'
#' Pretty printing for [`draws_list`] objects.
#'
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_iterations
#' @template args-print-max_chains
#' @template args-print-max_variables
#' @template args-methods-reserved
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_list(example_draws())
#' print(x)
#'
#' @export
print.draws_list <- function(x, digits = 2,
                             max_iterations = getOption("posterior.max_iterations", 10),
                             max_chains = getOption("posterior.max_chains", 2),
                             max_variables = getOption("posterior.max_variables", 4),
                             reserved = FALSE, ...) {
  max_iterations <- as_one_integer(max_iterations)
  max_chains <- as_one_integer(max_chains)
  max_variables <- as_one_integer(max_variables)
  reserved <- as_one_logical(reserved)
  niterations <- niterations(x)
  nchains <- nchains(x)
  nvariables <- nvariables(x)
  header <- paste0(
    "# A draws_list: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)
  sel_iterations <- seq_len(min(max_iterations, niterations))
  sel_chains <- seq_len(min(max_chains, nchains))
  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x
  if (!reserved) {
    y <- remove_reserved_variables(y)
  }
  y <- .subset_draws(
    y, sel_iterations, sel_chains, sel_variables,
    reserved = reserved
  )
  for (i in seq_along(y)) {
    cat0("\n[chain = ", i, "]\n")
    print(y[[i]], digits = digits, ...)
  }
  more_iterations <- niterations - max_iterations
  more_chains <- nchains - max_chains
  more_variables <- nvariables - max_variables
  if (more_iterations > 0 || more_chains > 0 || more_variables > 0) {
    comment <- character(0)
    if (more_iterations > 0) {
      comment <- c(comment, paste0(more_iterations, " more iterations"))
    }
    if (more_chains > 0) {
      comment <- c(comment, paste0(more_chains, " more chains"))
    }
    if (more_variables > 0) {
      comment <- c(comment, paste0(more_variables, " more variables"))
    }
    comment <- paste0(comment, collapse = ", and ")
    comment <- paste0("# ... with ", comment, "\n")
    cat(comment)
  }
  reserved_variables <- reserved_variables(x)
  if (!reserved && length(reserved_variables)) {
    cat0("# ... hidden reserved variables ", comma(reserved_variables), "\n")
  }
  invisible(x)
}

#' Print `draws_rvars` objects
#'
#' Pretty printing for [`draws_rvars`] objects.
#'
#' @encoding UTF-8
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_variables
#' @template args-print-summary
#' @template args-methods-reserved
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_rvars(example_draws())
#' print(x)
#'
#' @export
print.draws_rvars <- function(x,
  digits = 2,
  max_variables = getOption("posterior.max_variables", 8),
  summary = getOption("posterior.rvar_summary", "mean_sd"),
  reserved = FALSE,
  ...
) {
  max_variables <- as_one_integer(max_variables)
  niterations <- niterations(x)
  nchains <- nchains(x)
  nvariables <- nvariables(x)
  header <- paste0(
    "# A draws_rvars: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)

  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x
  if (!reserved) {
    y <- remove_reserved_variables(y)
  }
  y <- .subset_draws(y, variable = sel_variables, reserved = reserved)
  for (i in seq_along(y)) {
    cat0("$", names(y)[[i]], ": ")
    print(y[[i]], summary = summary, digits = digits, ...)
    cat("\n")
  }

  more_variables <- nvariables - max_variables
  if (more_variables > 0) {
    comment <- paste0(more_variables, " more variables")
    comment <- paste0("# ... with ", comment, "\n")
    cat(comment)
  }
  reserved_variables <- reserved_variables(x)
  if (!reserved && length(reserved_variables)) {
    cat0("# ... hidden reserved variables ", comma(reserved_variables), "\n")
  }

  invisible(x)
}

#' Print `draws_matrix` objects
#'
#' Pretty printing for [`draws_matrix`] objects.
#'
#' @template args-methods-x
#' @template args-print-digits
#' @template args-print-max_draws
#' @template args-print-max_variables
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_matrix(example_draws())
#' print(x)
#'
#' @export
print.draws_matrix <- function(x, digits = 2,
                               max_draws = getOption("max_draws", 10),
                               max_variables = getOption("max_variables", 8),
                               ...) {
  max_draws <- as_one_integer(max_draws)
  max_variables <- as_one_integer(max_variables)
  nvariables <- nvariables(x)
  ndraws <- ndraws(x)
  header <- paste0(
    "# A draws_matrix: ", ndraws,
    " draws, and ", nvariables, " variables\n"
  )
  cat(header)
  sel_draws <- seq_len(min(max_draws, ndraws))
  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x[sel_draws, sel_variables]
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
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_array(example_draws())
#' print(x)
#'
#' @export
print.draws_array <- function(x, digits = 2,
                              max_iterations = getOption("max_iterations", 5),
                              max_chains = getOption("max_chains", 8),
                              max_variables = getOption("max_variables", 4),
                              ...) {
  max_iterations <- as_one_integer(max_iterations)
  max_chains <- as_one_integer(max_chains)
  max_variables <- as_one_integer(max_variables)
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
  y <- x[sel_iterations, sel_chains, sel_variables]
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
#' @param meta_columns Logical. Show the meta-columns `.chain`,
#' `.iteration`, and `.draw` in printed output? Defaults to `FALSE`.
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_df(example_draws())
#' print(x)
#'
#' @export
print.draws_df <- function(x, digits = 2,
                           max_draws = getOption("max_draws", 10),
                           max_variables = getOption("max_variables", 8),
                           meta_columns = FALSE, ...) {
  max_draws <- as_one_integer(max_draws)
  max_variables <- as_one_integer(max_variables)
  meta_columns <- as_one_logical(meta_columns)
  niterations <- niterations(x)
  nchains <- nchains(x)
  ndraws <- ndraws(x)
  nvariables <- nvariables(x)
  header <- paste0(
    "# A draws_df: ", niterations, " iterations, ",
    nchains, " chains, and ", nvariables, " variables\n"
  )
  cat(header)
  meta_cols <- intersect(names(x), meta_columns())
  sel_draws <- seq_len(min(max_draws, ndraws))
  sel_variables <- seq_len(min(max_variables, nvariables))
  y <- x[sel_variables]
  if (meta_columns) {
    y <- cbind(y, x[, meta_cols])
  }
  y <- y[sel_draws, ]
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
  if (!meta_columns) {
    cat0("# ... hidden meta-columns ", comma(meta_cols), "\n")
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
#' @template args-print-dots
#' @template return-draws
#'
#' @examples
#' x <- as_draws_list(example_draws())
#' print(x)
#'
#' @export
print.draws_list <- function(x, digits = 2,
                             max_iterations = getOption("max_iterations", 10),
                             max_chains = getOption("max_chains", 2),
                             max_variables = getOption("max_variables", 4),
                             ...) {
  max_iterations <- as_one_integer(max_iterations)
  max_chains <- as_one_integer(max_chains)
  max_variables <- as_one_integer(max_variables)
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
  y <- x[sel_chains]
  for (i in seq_along(y)) {
    y[[i]] <- y[[i]][sel_variables]
    for (j in seq_along(y[[i]])) {
      y[[i]][[j]] <- y[[i]][[j]][sel_iterations]
    }
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
  invisible(x)
}

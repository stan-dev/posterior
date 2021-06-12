#' Mutate variables in `draws` objects
#'
#' Mutate variables in a [`draws`] object.
#'
#' @param .x (draws) A [`draws`] object.
#' @param ... Name-value pairs of expressions, each with either length 1 or the
#'   same length as in the entire input (i.e., number of iterations or draws).
#'   The name of each argument will be the name of a new variable, and the value
#'   will be its corresponding value. Use a `NULL` value in `mutate_variables`
#'   to drop a variable. New variables overwrite existing variables of the same
#'   name.
#'
#' @return
#' Returns a [`draws`] object of the same format as `.x`, with variables mutated
#' according to the expressions provided in `...`.
#'
#' @details
#' In order to mutate variables in [`draws_matrix`] and [`draws_array`] objects,
#' they are transformed to [`draws_df`] objects first and then transformed back
#' after mutation. As those transformations are quite expensive for larger
#' number of draws, we recommend using `mutate_variables` on [`draws_df`] and
#' [`draws_list`] objects if speed is an issue.
#'
#' In [`draws_rvars`] objects, the output of each expression in `...` is
#' coerced to an [`rvar`] object if it is not already one using `as_rvar()`.
#'
#' @seealso [`variables`], [`rename_variables`]
#'
#' @examples
#' x <- as_draws_df(example_draws())
#' x <- subset(x, variable = c("mu", "tau"))
#'
#' mutate_variables(x, tau2 = tau^2)
#' mutate_variables(x, scale = 1.96 * tau, lower = mu - scale)
#'
#' @importFrom rlang enquos caller_env eval_tidy as_label
#' @export
mutate_variables <- function(.x, ...) {
  UseMethod("mutate_variables")
}

#' @rdname mutate_variables
#' @export
mutate_variables.draws_matrix <- function(.x, ...) {
  as_draws_matrix(mutate_variables(as_draws_df(.x), ...))
}

#' @rdname mutate_variables
#' @export
mutate_variables.draws_array <- function(.x, ...) {
  as_draws_array(mutate_variables(as_draws_df(.x), ...))
}

#' @rdname mutate_variables
#' @export
mutate_variables.draws_df <- function(.x, ...) {
  dots <- enquos(..., .named = TRUE)
  names(dots) <- check_reserved_variables(names(dots))
  env <- caller_env()
  for (var in names(dots)) {
    .x[[var]] <- .mutate_variable(dots[[var]], .x, env)
  }
  .x
}

#' @rdname mutate_variables
#' @export
mutate_variables.draws_list <- function(.x, ...) {
  dots <- enquos(..., .named = TRUE)
  names(dots) <- check_reserved_variables(names(dots))
  env <- caller_env()
  for (chain in seq_along(.x)) {
    for (var in names(dots)) {
      .x[[chain]][[var]] <- .mutate_variable(dots[[var]], .x[[chain]], env)
    }
  }
  .x
}

#' @rdname mutate_variables
#' @export
mutate_variables.draws_rvars <- function(.x, ...) {
  dots <- enquos(..., .named = TRUE)
  names(dots) <- check_reserved_variables(names(dots))
  env <- caller_env()
  for (var in names(dots)) {
    .x[[var]] <- as_rvar(eval_tidy(dots[[var]], .x, env))
  }
  conform_rvar_ndraws_nchains(.x)
}

# evaluate an expression passed to 'mutate_variables' and check its validity
.mutate_variable <- function(expr, data, env = caller_env()) {
  out <- eval_tidy(expr, data, env)
  if (!is.numeric(out)) {
    stop_no_call("{", as_label(expr), "} does not evaluate to a numeric vector.")
  }
  n <- length(data[[1]])
  if (length(out) == 1L) {
    out <- rep(out, n)
  }
  if (length(out) != n) {
    stop_no_call("{", as_label(expr), "} does not evaluate ",
          "to a vector of length 1 or ", n, ".")
  }
  out
}

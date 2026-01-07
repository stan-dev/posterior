#' Extract array of a single (possibly indexed) variable
#'
#' Extract an array of draws of a single variable, including any dimensions of
#' variables with indices.
#'
#' @template args-methods-x
#' @param variable (string) The name of the variable to extract. To extract all
#' dimensions from variables with indices (e.g. `"x[1]"`), provide the base
#' variable name (e.g. `"x"`).
#' @param with_chains (logical) Should the array of draws include a dimension for chains?
#' If `TRUE` (the default), chains are included and the array has dimension
#' `c(niterations(x), nchains(x), ...)`. If `FALSE`, chains are not included and the array has
#' dimension `c(niterations(x) * nchains(x), ...)`.
#' @template args-methods-dots
#' @returns
#' An `array` with dimension `niterations(x)` x `nchains(x)` x any remaining
#' dimensions determined by the indices of the variable `x` (if `with_chains = TRUE`) or
#' dimension `niterations(x) * nchains(x)` x any remaining dimensions (if `with_chains = FALSE`).
#' @family variable extraction methods
#' @examples
#' x <- example_draws(example = "multi_normal")
#'
#' mu <- extract_variable_array(x, variable = "mu")
#' str(mu)
#'
#' # With chains collapsed
#' mu_no_chains <- extract_variable_array(x, variable = "mu", with_chains = FALSE)
#' str(mu_no_chains)
#'
#' mu1 <- extract_variable_array(x, variable = "mu[1]")
#' str(mu1)
#'
#' Sigma <- extract_variable_array(x, variable = "Sigma")
#' str(Sigma)
#'
#' @export
extract_variable_array <- function(x, variable, with_chains = TRUE, ...) {
  UseMethod("extract_variable_array")
}

#' @rdname extract_variable_array
#' @export
extract_variable_array.default <- function(x, variable, with_chains = TRUE, ...) {
  x <- as_draws(x)
  extract_variable_array(x, variable, with_chains = with_chains, ...)
}

#' @rdname extract_variable_array
#' @export
extract_variable_array.draws <- function(x, variable, with_chains = TRUE, ...) {
  variable <- as_one_character(variable)
  with_chains <- as_one_logical(with_chains)

  if (isTRUE(nzchar(split_variable_names(variable)$indices))) {
    # indices provided => scalar => equivalent to extract_variable_matrix
    out <- extract_variable_matrix(x, variable, ...)
    if (with_chains) {
      # Keep the original behavior: add a third dimension
      dim(out) <- c(dim(out), 1)
      dimnames(out) <- list(NULL)
    } else {
      # For with_chains = FALSE, reshape to (ndraws, 1) while preserving attributes
      out_reshaped <- array(unclass(out), dim = c(length(out), 1))
      dimnames(out_reshaped) <- list(NULL)
      if (is.factor(out)) {
        out <- copy_levels(out, out_reshaped)
      } else {
        out <- out_reshaped
      }
    }
  } else {
    x <- subset_draws(x, variable = variable, reserved = FALSE)
    x <- as_draws_rvars(x)
    out <- draws_of(x[[variable]], with_chains = with_chains)
  }

  out
}


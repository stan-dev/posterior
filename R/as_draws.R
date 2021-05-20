#' Transform to `draws` objects
#'
#' Try to transform an \R object to a format supported by the \pkg{posterior}
#' package.
#'
#' @name draws
#' @family formats
#'
#' @template args-methods-x
#' @template args-methods-dots
#'
#' @details The class `"draws"` is the parent class of all supported formats,
#'   which also have their own subclasses of the form `"draws_{format}"` (e.g.
#'   `"draws_array"`).
#'
#' @return If possible, a `draws` object in the closest supported format to `x`.
#'   The formats are linked to in the **See Also** section below.
#'
#' @examples
#' # create some random draws
#' x <- matrix(rnorm(30), nrow = 10)
#' colnames(x) <- c("a", "b", "c")
#' str(x)
#'
#' # transform to a draws object
#' y <- as_draws(x)
#' str(y)
#'
#' # remove the draws classes from the object
#' class(y) <- class(y)[-(1:2)]
#' str(y)
#'
NULL

#' @rdname draws
#' @export
as_draws <- function(x, ...) {
  UseMethod("as_draws")
}

#' @export
as_draws.draws <- function(x, ...) {
  x
}

#' @export
as_draws.array <- function(x, ...) {
  as_draws_array(x)
}

#' @export
as_draws.data.frame <- function(x, ...) {
  as_draws_df(x)
}

#' @export
as_draws.list <- function(x, ...) {
  as_draws_list(x)
}

#' @export
as_draws.matrix <- function(x, ...) {
  as_draws_matrix(x)
}

#' @export
as_draws.rvar <- function(x, ...) {
  as_draws_rvar(x)
}

#' @export
as_draws.mcmc <- function(x, ...) {
  as_draws_matrix(x)
}

#' @export
as_draws.mcmc.list <- function(x, ...) {
  as_draws_list(x)
}

#' @rdname draws
#' @export
is_draws <- function(x) {
  inherits(x, "draws")
}

# check if an object is supported by the posterior package
# the name 'check_draws' is already in use for checking
# the validity of the 'draw' argument in 'subset'
check_draws_object <- function(x) {
  if (!is_draws(x)) {
    stop_no_call("The object is not in a format supported by posterior.")
  }
  x
}

# define default variable names
# use the 'unique' naming strategy of tibble
# @param nvariables number of variables
default_variables <- function(nvariables) {
  paste0("...", seq_len(nvariables))
}

# validate draws vectors per variable
# @param ... Named arguments containing numeric vector
# @return a named list of numeric vectors
validate_draws_per_variable <- function(...) {
  out <- list(...)
  if (!rlang::is_named(out)) {
    stop_no_call("All variables must be named.")
  }
  if (".nchains" %in% names(out)) {
    # '.nchains' is an additional argument in chain supporting formats
    stop_no_call("'.nchains' is not supported for this format.")
  }
  out <- lapply(out, as.numeric)
  ndraws_per_variable <- lengths(out)
  ndraws <- max(ndraws_per_variable)
  if (!all(ndraws_per_variable %in% c(1, ndraws))) {
    stop_no_call("Number of draws per variable needs to be 1 or ", ndraws, ".")
  }
  for (i in which(ndraws_per_variable == 1)) {
    out[[i]] <- rep(out[[i]], ndraws)
  }
  out
}

#' Merge chains of `draws` objects
#'
#' Merge chains of [`draws`] objects into a single chain. Some operations will
#' trigger an automatic merging of chains, for example, because chains do not
#' match between two objects involved in a binary operation. By default, no
#' warning will be issued when this happens but you can activate one via
#' `options(posterior.warn_on_merge_chains = TRUE)`.
#'
#' @template args-methods-x
#' @template args-methods-dots
#' @template return-draws
#'
#' @examples
#' x <- example_draws()
#'
#' # draws_array with 4 chains, 100 iters each
#' str(x)
#'
#' # draws_array with 1 chain of 400 iterations
#' str(merge_chains(x))
#'
#' @export
merge_chains <- function(x, ...) {
  UseMethod("merge_chains")
}

#' @rdname merge_chains
#' @export
merge_chains.draws_matrix <- function(x, ...) {
  attr(x, "nchains") <- 1L
  x
}

#' @rdname merge_chains
#' @export
merge_chains.draws_array <- function(x, ...) {
  x <- merge_chains(as_draws_matrix(x))
  as_draws_array(x)
}

#' @rdname merge_chains
#' @export
merge_chains.draws_df <- function(x, ...) {
  x$.chain <- rep(1L, nrow(x))
  x$.iteration <- x$.draw
  x
}

#' @rdname merge_chains
#' @export
merge_chains.draws_list <- function(x, ...) {
  if (nchains(x) == 0) {
    return(x)
  }
  out <- empty_draws_list(variables(x), nchains = 1)
  for (v in variables(out)) {
    out[[1]][[v]] <- ulapply(x, "[[", v)
  }
  out
}

#' @rdname merge_chains
#' @export
merge_chains.rvar <- function(x, ...) {
  nchains_rvar(x) <- 1L
  x
}

#' @rdname merge_chains
#' @export
merge_chains.draws_rvars <- function(x, ...) {
  for (i in seq_along(x)) {
    x[[i]] <- merge_chains(x[[i]])
  }
  x
}

# some operations lead to an automatic chain merge
# that users can choose to be warned about
warn_merge_chains <- function(type = c("match", "index")) {
  warn <- as_one_logical(getOption(
    "posterior.warn_on_merge_chains",
    default = FALSE
  ))
  if (warn) {
    type <- as_one_character(type)
    warning_no_call(
      "Chains were dropped",
      switch(type, ".",
        match = " due to chain information not matching.",
        index = " due to manually indexing draws."
      )
    )
  }
  invisible(NULL)
}

#' Split Chains
#'
#' Split chains by halving the number of iterations per chain and doubling the
#' number of chains.
#'
#' @template args-methods-x
#' @template args-methods-dots
#' @template return-draws
#'
#' @examples
#' x <- example_draws()
#' niterations(x)
#' nchains(x)
#'
#' x <- split_chains(x)
#' niterations(x)
#' nchains(x)
#'
#' @export
split_chains <- function(x, ...) {
  UseMethod("split_chains")
}

#' @export
split_chains.draws <- function(x, ...) {
  niter <- niterations(x)
  if (niter %% 2 != 0) {
    warning_no_call(
      "Number of iterations is not even. Removing the last iteration ",
      "in order to split chains into two parts of equal length."
    )
    niter <- niter - 1
  }
  iter_first_half <- seq_len(floor(niter / 2))
  iter_second_half <- seq_len(niter)[-iter_first_half]
  x_first_half <- .subset_draws(x, iteration = iter_first_half)
  x_second_half <- .subset_draws(x, iteration = iter_second_half)
  bind_draws(x_first_half, x_second_half, along = "chain")
}


# internal ----------------------------------------------------------------

# split chains for use in convergence diagnostics
# @param x matrix of draws (iterations x chains)
# @return matrix of draws with split chains
.split_chains <- function(x) {
  x <- as.matrix(x)
  niter <- NROW(x)
  if (niter == 1L) {
    return(x)
  }
  half <- niter / 2
  cbind(x[1:floor(half), ], x[ceiling(half + 1):niter, ])
}

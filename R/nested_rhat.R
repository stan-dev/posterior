#' Nested Rhat convergence diagnostic
#'
#' Compute the Nested Rhat convergence diagnostic for a single variable
#' proposed in Margossian et al. (2023).
#'
#' @family diagnostics
#' @template args-conv
#' @param superchain_ids (numeric) Vector of length nchains specifying
#'   which superchain each chain belongs to
#' @template args-methods-dots
#' @template return-conv
#' @template ref-margossian-nestedrhat-2023
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat_nested(mu, superchain_ids = c(1,1,2,2))
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' rhat(d$Sigma, superchain_ids = c(1,1,2,2))
#'
#' @export
rhat_nested <- function(x, superchain_ids, ...) UseMethod("rhat_nested")

#' @rdname rhat_nested
#' @export
rhat_nested.default <- function(x, superchain_ids, ...) {
  .rhat_nested(x, superchain_ids = superchain_ids)
}

#' @rdname rhat_nested
#' @export
rhat_nested.rvar <- function(x, superchain_ids, ...) {
  summarise_rvar_by_element_with_chains(x, rhat_nested, superchain_ids = superchain_ids, ...)
}


.rhat_nested <- function(x, superchain_ids, ...) {

  array_dims <- dim(x)
  ndraws <- array_dims[1]
  nchains_per_superchain <- max(table(superchain_ids))
  nsuperchains <- length(unique(superchain_ids))

  superchain_mean <- sapply(unique(superchain_ids), function(k) mean(x[, which(superchain_ids == k)]))

  chain_mean <- matrix(matrixStats::colMeans2(x), nrow = 1)
  chain_var <- matrixStats::colVars(x, center=chain_mean)
  
  overall_mean <- mean(superchain_mean)

  if (nchains_per_superchain == 1) {
    var_between_chain <- 0
  } else {
    var_between_chain <- sapply(unique(superchain_ids), function(k) var(chain_mean[, which(superchain_ids == k)]))
  }
  if (ndraws == 1) {
    var_within_chain <- 0
  } else {
    var_within_chain <- sapply(unique(superchain_ids), function(k) mean(chain_var[which(superchain_ids == k)]))
  }
  
  var_between_superchain <- matrixStats::colVars(
    as.matrix(superchain_mean),
    center = overall_mean
  )
  
  var_within_superchain <- mean(var_within_chain + var_between_chain)

  sqrt(1 + var_between_superchain / var_within_superchain)  
}

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
#' rhat_nested(d$Sigma, superchain_ids = c(1,1,2,2))
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

  x <- as.matrix(x)
  niterations <- NROW(x)
  nchains_per_superchain <- max(table(superchain_ids))
  superchains <- unique(superchain_ids)

  # mean and variance of chains calculated as in rhat
  chain_mean <- matrixStats::colMeans2(x)
  chain_var <- matrixStats::colVars(x, center = chain_mean)

  # mean of superchains calculated by only including specified chains
  # (equation 15 in Margossian et al. 2023)
  superchain_mean <- sapply(
    superchains,
    function(k) mean(x[, which(superchain_ids == k)])
  )

  # overall mean (as defined in equation 16 in Margossian et al. 2023)
  overall_mean <- mean(superchain_mean)

  # between-chain variance estimate (B_k in equation 18 in Margossian et al. 2023)
  if (nchains_per_superchain == 1) {
    var_between_chain <- 0
  } else {
    var_between_chain <- sapply(
      superchains,
      function(k) var(chain_mean[which(superchain_ids == k)])
    )
  }

  # within-chain variance estimate (W_k in equation 18 in Margossian et al. 2023)
  if (niterations == 1) {
    var_within_chain <- 0
  } else {
    var_within_chain <- sapply(
      superchains,
      function(k) mean(chain_var[which(superchain_ids == k)])
    )
  }

  # between-superchain variance (nB in equation 17 in Margossian et al. 2023)
  var_between_superchain <- matrixStats::colVars(
    as.matrix(superchain_mean),
    center = overall_mean
  )

  # within-superchain variance (nW in equation 18 in Margossian et al. 2023)
  var_within_superchain <- mean(var_within_chain + var_between_chain)

  # nested Rhat (nRhat in equation 19 in Margossian et al. 2023)
  sqrt(1 + var_between_superchain / var_within_superchain)
}

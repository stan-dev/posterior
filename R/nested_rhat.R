#' Nested Rhat convergence diagnostic
#'
#' Compute the nested Rhat convergence diagnostic for a single
#' variable as proposed in Margossian et al. (2024).
#'
#' @family diagnostics
#' @template args-conv
#' @param superchain_ids (numeric) Vector of length nchains specifying
#'   which superchain each chain belongs to. There should be equal
#'   numbers of chains in each superchain. All chains within the same
#'   superchain are assumed to have been initialized at the same
#'   point.
#' @template args-methods-dots
#'
#' @details Nested Rhat is a convergence diagnostic useful when
#'   running many short chains. It is calculated on superchains, which
#'   are groups of chains that have been initialized at the same
#'   point.
#'
#' Note that there is a slight difference in the calculation of Rhat
#'   and nested Rhat, as nested Rhat is lower bounded by 1. This means
#'   that nested Rhat with one chain per superchain will not be
#'   exactly equal to basic Rhat (see Footnote 3 in Margossian et
#'   al. (2024)).
#'
#' @template return-conv
#' @template ref-margossian-nestedrhat-2024
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' rhat_nested(mu, superchain_ids = c(1, 1, 2, 2))
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' rhat_nested(d$Sigma, superchain_ids = c(1, 1, 2, 2))
#'
#' @export
rhat_nested <- function(x, ...) UseMethod("rhat_nested")

#' @rdname rhat_nested
#' @export
rhat_nested.default <- function(x, superchain_ids, ...) {
  .rhat_nested(x, superchain_ids = superchain_ids)
}

#' @rdname rhat_nested
#' @export
rhat_nested.rvar <- function(x, superchain_ids, ...) {
  summarise_rvar_by_element_with_chains(
    x, rhat_nested, superchain_ids = superchain_ids, ...
  )
}

.rhat_nested <- function(x, superchain_ids, ...) {
  if (should_return_NA(x)) {
    return(NA_real_)
  }

  x <- as.matrix(x)
  niterations <- NROW(x)
  nchains <- NCOL(x)

  # check that all chains are assigned a superchain
  if (length(superchain_ids) != nchains) {
    warning_no_call("Length of superchain_ids not equal to number of chains, ",
                    "returning NA.")
    return(NA_real_)
  }

  # check that superchains are equal length
  superchain_id_table <- table(superchain_ids)
  nchains_per_superchain <- max(superchain_id_table)

  if (nchains_per_superchain != min(superchain_id_table)) {
    warning_no_call("Number of chains per superchain is not the same for ",
                    "each superchain, returning NA.")
    return(NA_real_)
  }

  superchains <- unique(superchain_ids)

  # mean and variance of chains calculated as in Rhat
  chain_mean <- matrixStats::colMeans2(x)
  chain_var <- matrixStats::colVars(x, center = chain_mean)

  # mean of superchains calculated by only including specified chains
  # (equation 4 in Margossian et al. 2024)
  superchain_mean <- sapply(
    superchains, function(k) mean(x[, which(superchain_ids == k)])
  )

  # between-chain variance estimate (Bhat_k in equation 7 in Margossian et al. 2024)
  if (nchains_per_superchain == 1) {
    var_between_chain <- 0
  } else {
    var_between_chain <- sapply(
      superchains,
      function(k) var(chain_mean[which(superchain_ids == k)])
    )
  }

  # within-chain variance estimate (What_k in equation 7 in Margossian et al. 2024)
  if (niterations == 1) {
    var_within_chain <- 0
  } else {
    var_within_chain <- sapply(
      superchains,
      function(k) mean(chain_var[which(superchain_ids == k)])
    )
  }

  # between-superchain variance (Bhat_nu in equation 6 in Margossian et al. 2024)
  var_between_superchain <- var(superchain_mean)

  # within-superchain variance (What_nu in equation 7 in Margossian et al. 2024)
  var_within_superchain <- mean(var_within_chain + var_between_chain)

  # nested Rhat (Rhat_nu in equation 8 in Margossian et al. 2024)
  sqrt(1 + var_between_superchain / var_within_superchain)
}

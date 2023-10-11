.add_superchain_ids <- function(draws, superchain_ids) {

  # determine size of dims
  chains_per_superchain <- table(superchain_ids)
  num_chains_per_superchain <- max(chains_per_superchain)
  num_iterations <- dim(draws)[1]
  num_superchains <- max(superchain_ids)

  # create new empty array with correct dims
  new_draws <- array(
    NA,
    dim = c(
      num_iterations,
      num_chains_per_superchain,
      num_superchains)
  )

  # add dim names
  dimnames(new_draws) <- list(
    iteration = 1:num_iterations,
    chain = 1:num_chains_per_superchain,
    superchain = 1:num_superchains
  )

  # assign chains to superchains
  for (k in 1:num_superchains) {
    chains_in_superchain <- which(superchain_ids == k)
    new_draws[, , k] <- draws[, chains_in_superchain]
  }

  return(new_draws)
}

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

  x <- .add_superchain_ids(x, superchain_ids)
  .rhat_nested(x)
}

.rhat_nested <- function(x, ...) {

  array_dims <- dim(x)
  ndraws <- array_dims[1]
  nchains <- array_dims[2]
  nsuperchains <- array_dims[3]
  
  superchain_mean <- apply(x, 3, mean)
  chain_mean <- apply(x, c(2, 3), mean)
  chain_var <- apply(x, c(2, 3), var)
  
  overall_mean <- mean(superchain_mean)

  if (nchains == 1) {
    var_between_chain <- 0
  } else {
    var_between_chain <- matrixStats::colVars(
      chain_mean,
      center = superchain_mean
    )
  }
  
  if (ndraws == 1) {
    var_within_chain <- 0
  } else {
    var_within_chain <- colMeans(chain_var)
  }
  
  var_between_superchain <- matrixStats::colVars(
    as.matrix(superchain_mean),
    center = overall_mean
  )
  
  var_within_superchain <- mean(var_within_chain + var_between_chain)

  sqrt(1 + var_between_superchain / var_within_superchain)
}

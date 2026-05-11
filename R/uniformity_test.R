#' Uniformity test for PIT values
#'
#' Tests whether PIT (probability integral transform) values deviate from
#' uniformity while accounting for dependence among them. LOO-PIT values are
#' not independent because each LOO predictive distribution is conditioned on
#' nearly the same data (all but one observation), which induces correlation.
#' Standard uniformity tests that assume independence can have inflated
#' Type I error or reduced power. This function implements dependence-aware
#' procedures from Tesso and Vehtari (2026).
#'
#' Three test variants are available:
#'
#' * **POT** (Pointwise Order Tests): Uses beta distributions for order
#'   statistics. Best for continuous PIT values; recommended as default for
#'   LOO-PIT model checking. Good power against diverse departures from
#'   uniformity.
#' * **PIET** (Pointwise Inverse-CDF Evaluation Tests): Uses a continuous
#'   reference distribution (exponential) via inverse-CDF transformation.
#'   Best for detecting tail deviations; maintains valid Type I error control
#'   under strong dependence.
#' * **PRIT** (Pointwise Rank-based Individual Tests): Uses binomial
#'   distributions on scaled ECDF (ranks). Intended for discrete or
#'   rank-based PIT values.
#'
#' All procedures compute pointwise p-values, aggregate them via the (truncated)
#' Cauchy combination test (Liu and Xie, 2020; Chen et al., 2025), and derive
#' Shapley values to quantify each point's contribution to the overall test.
#'
#' @param pit Numeric vector of PIT values in `[0, 1]`.
#' @param test Character string. One of `"POT"`, `"PIET"`, or `"PRIT"`.
#'   See details above.
#' @param truncate Boolean. Determines whether the truncated (TRUE) or
#'   untruncated (FALSE) Cauchy combination test (CCT) is used. By default it
#'   is NULL in which case the truncated CCT is computed for `POT` and `PRIT`
#'   and the untruncated CCT is computed for `PIET`.
#'
#' @return A list with components:
#'   * `pvalue`: Global p-value from the Cauchy combination test.
#'   * `pointwise`: Shapley values (contributions of each PIT value to the
#'     test statistic). Non-negative values indicate points contributing to
#'     evidence against uniformity.
#'
#' @references
#'   Tesso, H., and Vehtari, A. (2026). LOO-PIT predictive model checking.
#'   arXiv preprint arXiv:2603.02928.
#'
#'   Liu, Y., and Xie, J. (2020). Cauchy combination test: a powerful test
#'   with analytic p-value calculation under arbitrary dependency structures.
#'   *Journal of the American Statistical Association*, 115(529), 393-402.
#'
#'   Chen, B., Xu, W., and Gao, X. (2025). Truncated Cauchy combination test:
#'   a robust and powerful p-value combination method with arbitrary
#'   correlations. arXiv preprint arXiv:2506.12489.
#'
#' @export
uniformity_test <- function(pit, test, truncate = NULL) {

  test <- rlang::arg_match(test, values = c("POT", "PRIT", "PIET"))
  if (is.null(truncate)) truncate <- (test != "PIET")

  test_fn <- switch(test,
      POT  = .pot_test,
      PIET = .piet_test,
      PRIT = .prit_test
    )

    pit_sorted           <- sort(pit)
    std_cauchy_values    <- .compute_cauchy(test_fn(pit_sorted))
    p_value_CCT          <- .cauchy_combination_test(
      test_fn(pit),
      truncate = truncate
    )
    pointwise_contributions  <- .compute_shapley_values(std_cauchy_values)

    res <- list()
    res$pvalue      <- p_value_CCT
    res$pointwise   <- pointwise_contributions

    return(res)
}


# internal ----------------------------------------------------------------

#' Compute Shapley values
#'
#' Calculates the average marginal contribution of players across
#' all random arrival orders in a cooperative game.
#' Used to provide a principled approach for quantifying
#' point-specific influences in a way that reflects local miscalibration.
#'
#' @param x Numeric vector of Cauchy-transformed PIT values.
#' @return Numeric vector of Shapley values with the same length as `x`.
#' @noRd
.compute_shapley_values <- function(x) {
  n <- length(x)
  if (n == 0) {
    return(numeric(0))
  }
  if (n == 1) {
    return(0)
  }

  # Harmonic number
  # H_n = sum(1/i) for i = 1 to n
  harmonic_number <- sum(1 / seq_len(n))

  shapley_values <- numeric(n)
  for (i in seq_len(n)) {
    mean_others <- sum(x[-i]) / (n - 1)
    # Applies the closed-form formula to assign player i their fair share.
    shapley_values[i] <- (1 / n) * x[i] + ((harmonic_number - 1) / n) * (x[i] - mean_others)
  }

  return(shapley_values)
}

#' Pointwise Inverse-CDF Evaluation Tests Combination (PIET)
#'
#' Uniformity test with respect to any continuous distribution.
#' H0: The value obtained via the inverse CDF transformation F^(-1)(x_i)
#' follows the distribution of X under uniformity.
#' HA: The p-value p_(i) provides evidence against uniformity.
#'
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.piet_test <- function(x) {
  cdf_exp <- pexp(-log(x), rate = 1) # same as 1-x but numerically more stable
  p_values <- 2 * pmin(cdf_exp, 1 - cdf_exp)

  return(p_values)
}

#' Pointwise Order Tests Combination (POT)
#'
#' Uniformity test based on a beta distribution.
#' H0: The i-th order statistic u_(i) follows a beta distribution
#' under uniformity.
#' HA: The p-value p_(i) provides evidence against uniformity
#' at the i-th order statistic u_(i).
#'
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.pot_test <- function(x) {
  n <- length(x)
  # keep NA values instead of silent recycling via sort()
  # TODO: Can PIT values be NAN at this point?
  cdf_beta <- pbeta(sort(x, na.last = TRUE), 1:n, seq(n, 1, by = -1))
  p_values <- 2 * pmin(cdf_beta, 1 - cdf_beta)

  return(p_values)
}

#' Pointwise Rank-based Individual Tests Combination (PRIT)
#'
#' Uniformity test based on a binomial distribution.
#' H0: The number of observations falling at or below x_i
#' follows a binomial distribution under uniformity.
#' HA: The p-value p_i provides evidence against uniformity.
#'
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of p-values.
#' @noRd
.prit_test <- function(x) {
  n <- length(x)
  scaled_ecdf <- n * ecdf(x)(x)
  probs1 <- pbinom(scaled_ecdf - 1, n, x)
  probs2 <- pbinom(scaled_ecdf, n, x)
  p_values <- 2 * pmin(1 - probs1, probs2)

  return(p_values)
}

#' Truncated Cauchy combination test
#'
#' Combines dependent p-values using the Cauchy combination method.
#' If truncate, only p-values less than 0.5 are included.
#'
#' @param x Numeric vector of p-values transformed to follow a standard
#' Cauchy distribution.
#' @param truncate Boolean; If TRUE only p-values less than 0.5 are
#' included.
#' @return p-value of the Cauchy combination method.
#' @noRd
.cauchy_combination_test <- function(x, truncate = NULL) {
  if (truncate) {
    idx <- which(x < 0.5)
    if (length(idx) == 0) {
      stop("Cannot compute truncated Cauchy combination test. ",
           "No p-values below 0.5 found.")
    }
    1 - pcauchy(mean(-qcauchy(x[idx])))
  } else {
    1 - pcauchy(mean(-qcauchy(x)))
  }
}

#' Compute Cauchy transformation
#'
#' Transforms PIT values to follow a standard Cauchy distribution.
#'
#' @param x Numeric vector of PIT values in `[0, 1]`.
#' @return Numeric vector of Cauchy-transformed values.
#' @noRd
.compute_cauchy <- function(x) {
  tan((0.5 - x) * pi)
}
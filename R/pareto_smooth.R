##' Pareto-smoothing minimum sample-size
##'
#' Given Pareto-k computes the minimum sample size for reliable Pareto
#' smoothed estimate (to have small probability of large error)
##' @param k
##' @return minimum sample size
ps_min_ss <- function(k) {
  # Eq (11) in PSIS paper
  10^(1 / (1 - max(0, k)))
}



##' Pareto-smoothing k-hat threshold
##'
##' Given sample size S computes khat threshold for reliable Pareto
##' smoothed estimate (to have small probability of large error)
##' @param S sample size
##' @return threshold
ps_khat_threshold <- function(S) {
  1 - 1 / log10(S)
}



##' Pareto-smoothing convergence rate
##'
##' Given S and scalar or array of k's, compute the relative
##' convergence rate of PSIS estimate RMSE
##' @param k pareto-k values
##' @param S sample size
##' @return convergence rate
ps_convergence_rate <- function(k, S) {
  # allow array of k's
  rate <- numeric(length(k))
  # k<0 bounded distribution
  rate[k < 0] <- 1
  # k>0 non-finite mean
  rate[k > 1] <- 0
  # limit value at k=1/2
  rate[k == 0.5] <- 1 - 1 / log(S)
  # smooth approximation for the rest (see Appendix of PSIS paper)
  ki <- (k > 0 & k < 1 & k != 0.5)
  kk <- k[ki]
  rate[ki] <- pmax(
    0,
    (2 * (kk - 1) * S^(2 * kk + 1) + (1 - 2 * kk) * S^(2 * kk) + S^2) /
      ((S - 1) * (S - S^(2 * kk)))
  )
  rate
}


##' Pareto-k diagnostic message
##'
##' Given S and scalar and k, form a diagnostic message string
##' @param k pareto-k values
##' @param S sample size
##' @return diagnostic message
pareto_k_diagmsg <- function(k, S) {
  msg <- paste0('With k=', round(k,2), ' and S=', round(S,0), ':\n')
  if (k > 1) {
    msg <- paste0(msg,'All estimates are unreliable. If the distribution of ratios is bounded,\nfurther draws may improve the estimates, but it is not possible to predict\nwhether any feasible sample size is sufficient.')
  } else {
    if (k > ps_khat_threshold(S)) {
      msg <- paste0(msg, 'S is too small, and sample size larger than ', round(ps_min_ss(k),0), ' is neeeded for reliable results.\n')
    } else {
      msg <- paste0(msg, 'To halve the RMSE, approximately ', round(2^(2/ps_convergence_rate(k,S)),1), ' times bigger S is needed.\n')
    }
  }
  if (k > 0.7 && k < 1) {
    msg <- paste0(msg, 'Bias dominates RMSE, and the variance based MCSE is underestimated.\n')
  }
  message(msg)
}

##' Pareto-khat
##'
##' Calculate pareto-khat value given draws
##' @template args-method-x
##' @param ndraws_tail (numeric) number of draws for the tail. Default is max(ceiling(3 * sqrt(length(draws))), S / 5)
##' @param tail which tail
##' @param r_eff relative effective. Default is "auto"
##' @param verbose (logical) Should a diagnostic message be displayed? Default is `TRUE`.
##' @template args-methods-dots
##' @return List of Pareto-smoothing diagnostics
pareto_khat <- function(x, ...) {
  UseMethod("pareto_khat")
}

# To do ----
#   - use r_eff (scalar, vector, or NULL to compute r_eff from draws)
#   - support "left" and both "tails"

# Question:
#   - if draws is a draws matrix or similar object with multiple parameters
#   and r_eff is a vector, then how to structure the output?


pareto_khat.default <- function(x, ...) {
  x <- as_draws(x)
  pareto_khat(x, ...)
}


pareto_khat.draws <- function(x,
                              ndraws_tail = "default",
                              tail = c("right", "left", "both"),
                              r_eff = NULL,
                              verbose = TRUE) {

  if (is.null(r_eff)) {
    r_eff <- 1 # TODO: calculate this
  }

  S <- length(x)

  if (ndraws_tail == "default") {
    ndraws_tail <- max(ceiling(3 * sqrt(length(x))), S / 5)
  }

  tail <- match.arg(tail)

  if (ndraws_tail >= 5) {
    ord <- sort.int(x, index.return = TRUE)
    tail_ids <- seq(S - ndraws_tail + 1, S)
    draws_tail <- ord$x[tail_ids]
    if (abs(max(draws_tail) - min(draws_tail)) < .Machine$double.eps / 100) {
      warning(
        "Can't fit generalized Pareto distribution ",
        "because all tail values are the same.",
        call. = FALSE
      )
    } else {
      cutoff <- ord$x[min(tail_ids) - 1] # largest value smaller than tail values
      # save time not sorting since x already sorted
      fit <- gpdfit(draws_tail - cutoff, sort_x = FALSE)
      k <- fit$k
      sigma <- fit$sigma
      if (is.finite(k)) {
        p <- (seq_len(ndraws_tail) - 0.5) / ndraws_tail
        tail <- qgpd(p = p, mu = cutoff, k = k, sigma = sigma)
      } else {
        tail <- draws_tail
      }
      x[ord$ix[tail_ids]] <- tail
    }
  } else {
    warning(
      "Can't fit generalized Pareto distribution ",
      "because ndraws_tail is less than 5.",
      call. = FALSE
    )
  }

  # truncate at max of raw draws
  x[x > ord$x[S]] <- ord$x[S]

  min_ss <- ps_min_ss(k)

  khat_threshold <- ps_khat_threshold(S)

  convergence_rate <- ps_convergence_rate(k, S)

  if (verbose) {
    pareto_k_diagmsg(k, S)
  }

  list(
    "k" = k,
    "sigma" = sigma,
    "min_ss" = min_ss,
    "khat_threshold" = khat_threshold,
    "convergence_rate" = convergence_rate
  )
}

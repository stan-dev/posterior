##' Pareto-smoothing minimum sample-size
##'
#' Given Pareto-k computes the minimum sample size for reliable Pareto
#' smoothed estimate (to have small probability of large error)
##' @param k
##' @param ... unused
##' @return minimum sample size
ps_min_ss <- function(k, ...) {
  # Eq (11) in PSIS paper
  10^(1 / (1 - max(0, k)))
}



##' Pareto-smoothing k-hat threshold
##'
##' Given sample size S computes khat threshold for reliable Pareto
##' smoothed estimate (to have small probability of large error). See
##' section 3.2.4, equation (13).
##' @param S sample size
##' @param ... unused
##' @return threshold
ps_khat_threshold <- function(S, ...) {
  1 - 1 / log10(S)
}



##' Pareto-smoothing convergence rate
##'
##' Given S and scalar or array of k's, compute the relative
##' convergence rate of PSIS estimate RMSE
##' @param k pareto-k values
##' @param S sample size
##' @param ... unused
##' @return convergence rate
ps_convergence_rate <- function(k, S, ...) {
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
##' @param ... unused
##' @return diagnostic message
pareto_k_diagmsg <- function(diags, ...) {
  k <- diags$k
  sigma <- diags$sigma
  min_ss <- diags$min_ss
  khat_threshold <- diags$khat_threshold
  convergence_rate <- diags$convergence_rate
  msg <- NULL
  if (k > 1) {
    msg <- paste0(msg,'All estimates are unreliable. If the distribution of ratios is bounded,\n',
                  'further draws may improve the estimates, but it is not possible to predict\n',
                  'whether any feasible sample size is sufficient.')
  } else {
    if (k > khat_threshold) {
      msg <- paste0(msg, 'S is too small, and sample size larger than ', round(min_ss, 0), ' is neeeded for reliable results.\n')
    } else {
      msg <- paste0(msg, 'To halve the RMSE, approximately ', round(2^(2/convergence_rate),1), ' times bigger S is needed.\n')
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
##' @template args-methods-x
##' @param x object for which method is defined
##' @param ndraws_tail (numeric) number of draws for the tail. Default
##'   is max(ceiling(3 * sqrt(length(draws))), S / 5) (Supplementary
##'   H)
##' @param tail which tail
##' @param r_eff relative effective. Default is "auto"
##' @param verbose (logical) Should a diagnostic message be displayed?
##'   Default is `TRUE`.
##' @param extra_diags include extra pareto-k diagnostics?
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

pareto_khat.default <- function(x,
                                ndraws_tail = "default",
                                tail = c("both", "right", "left"),
                                r_eff = NULL,
                                verbose = FALSE,
                                extra_diags = FALSE, ...) {

  tail <- match.arg(tail)

  if (tail == "both") {
    # left first
    original_x <- x
    x <- -x    

    khat_left <- .pareto_khat(
      x,
      ndraws_tail = ndraws_tail,
      r_eff = r_eff,
      extra_diags = extra_diags
    )

    khat_right <- .pareto_khat(
      original_x,
      ndraws_tail = ndraws_tail,
      r_eff = r_eff,
      extra_diags = extra_diags
    )

    # take max of khats and corresponding diagnostics
    k <- max(khat_right, khat_left)

  }  else {

    if (tail == "left") {
      # flip sign to do left tail
      
      original_x <- x
      x <- -x
    }

    k <- .pareto_khat(
      x,
      ndraws_tail = ndraws_tail,
      r_eff = r_eff
    )
    
  }

  out <- list(pareto_k = k)

  if (extra_diags) {
    diags <- .pareto_k_extra_diags(k, length(x))
    out <- c(out, diags)
  }

  if (verbose) {
    if (!extra_diags) {
      diags <- .pareto_k_extra_diags(k, length(x))
      }
    pareto_k_diagmsg(
      k = k,
      diags = diags
    )
  }

  out

}

.pareto_k_extra_diags <- function(k, S, ...) {

    min_ss <- ps_min_ss(k)

    khat_threshold <- ps_khat_threshold(S)

    convergence_rate <- ps_convergence_rate(k, S)

    other_diags <- list(
#      "sigma" = sigma,
      "min_ss" = min_ss,
      "khat_threshold" = khat_threshold,
      "convergence_rate" = convergence_rate
    )
}

.pareto_khat <- function(x,
                         ndraws_tail = "default",
                         r_eff = NULL,
                         extra_diags = FALSE) {

  if (is.null(r_eff)) {
    r_eff <- 1 # TODO: calculate this
  }

  S <- length(x)

  if (ndraws_tail == "default") {
    # Appendix H in PSIS paper
    ndraws_tail <- ifelse(S > 225, ceiling(3 * sqrt(S/r_eff)), S / 5)
  }

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
        draws_tail <- qgpd(p = p, mu = cutoff, k = k, sigma = sigma)
      }
      x[ord$ix[tail_ids]] <- draws_tail
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

  k
}

pareto_smooth_tail <- function(x, cutoff) {

  len <- length(x)

  # save time not sorting since x already sorted
  fit <- gpdfit(x - cutoff, sort_x = FALSE)
  k <- fit$k
  sigma <- fit$sigma
  if (is.finite(k)) {
    p <- (seq_len(len) - 0.5) / len
    qq <- qgpd(p, k, sigma) + cutoff
    tail <- log(qq)
  } else {
    tail <- x
  }
  list(tail = tail, k = k)
}

#' Pareto khat diagnostic
#'
#' Estimate Pareto k value from Pareto smoothing the tail(s) of x. For
#' further details see Vehtari et al. (2022).
#'
#' @template args-pareto
#' @template args-methods-dots
#' @return numeric vector of Pareto smoothing diagnostics
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' pareto_khat(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' pareto_khat(d$Sigma)
#' @export
pareto_khat <- function(x, ...) UseMethod("pareto_khat")

#' @rdname pareto_khat
#' @export
pareto_khat.default <- function(x,
                                ...) {
  pareto_smooth.default(x, return_smoothed = FALSE, ...)
}

#' @rdname pareto_khat
#' @export
pareto_khat.rvar <- function(x, ...) {
  unlist(
    summarise_rvar_by_element_with_chains(x, pareto_khat.default, ...),
    recursive = FALSE
  )
}

#' Pareto smoothing
#'
#' Smooth the tail(s) of x by fitting a generalized Pareto
#' distribution. For further details see Vehtari et al. (2022).
#'
#' @template args-pareto
#' @template args-methods-dots
#' @param return_smoothed (logical) Should the smoothed x be returned?
#'   If `TRUE`, returns x with smoothed tail(s). If `FALSE`, acts the
#'   same as `pareto_khat`. Default is `TRUE`.
#' @return List containing vector of smoothed values and vector of
#'   pareto smoothing diagnostics.
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' pareto_smooth(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' pareto_smooth(d$Sigma)
#' @export
pareto_smooth <- function(x, ...) UseMethod("pareto_smooth")

#' @rdname pareto_smooth
#' @export
pareto_smooth.rvar <- function(x, ...) {
    unlist(summarise_rvar_by_element_with_chains(x, pareto_smooth.default, ...), recursive = FALSE)
}

#' @rdname pareto_smooth
#' @export
pareto_smooth.default <- function(x,
                                  tail = c("both", "right", "left"),
                                  r_eff = NULL,
                                  ndraws_tail = NULL,
                                  extra_diags = FALSE,
                                  verbose = FALSE,
                                  return_smoothed = TRUE,
                                  ...) {

  # check for infinite or na values
  if (should_return_NA(x)) {
    return(NA_real_)
  }

  tail <- match.arg(tail)
  S <- length(x)
  
  # automatically calculate relative efficiency
  if (is.null(r_eff)) {
    r_eff <- ess_basic(x) / S
  }

  # automatically calculate tail length
  if (is.null(ndraws_tail)) {
    ndraws_tail <- ps_tail_length(S, r_eff)
  }
  
  if (tail == "both") {
    
    if (ndraws_tail > S / 2) {
      warning("Number of tail draws cannot be more than half ",
              "the length of the draws if both tails are fit, ",
              "changing to ", S / 2, ".")
      ndraws_tail <- S / 2
    }

    if (ndraws_tail < 5) {
      warning("Number of tail draws cannot be less than 5. ",
              "Changing to ", 5, ".")
      ndraws_tail <- 5
    }

    # left tail
    smoothed <- .pareto_smooth_tail(
      x,
      ndraws_tail = ndraws_tail,
      tail = "left"
    )
    left_k <- smoothed$k

    # right tail
    smoothed <-.pareto_smooth_tail(
      x = smoothed$x,
      ndraws_tail = ndraws_tail,
      tail = "right"
    )
    right_k <- smoothed$k

    k <- max(left_k, right_k)
    x <- smoothed$x
  } else {
    
    smoothed <- .pareto_smooth_tail(
      x,
      ndraws_tail = ndraws_tail,
      tail = tail
    )
    k <- smoothed$k
    x <- smoothed$x
  }

  diags <- list(khat = k)

  if (extra_diags) {
    ext_diags <- .pareto_smooth_extra_diags(k, S)
    diags <- c(diags, ext_diags)
  }

  if (verbose) {
    if (!extra_diags) {
      diags <- .pareto_smooth_extra_diags(diags$khat, length(x))
    }
    pareto_k_diagmsg(
      diags = diags
    )
  }
  if (return_smoothed) {
    out <- list(x = x, diagnostics = diags)
  } else {
    out <- diags
  }
  return(out)
}

.pareto_smooth_tail <- function(x,
                                ndraws_tail,
                                tail = c("right", "left"),
                                ...
                                ) {

  tail <- match.arg(tail)

  S <- length(x)
  tail_ids <- seq(S - ndraws_tail + 1, S)
  
  
  if (tail == "left") {
    x <- -x
  }

  ord <- sort.int(x, index.return = TRUE)
  draws_tail <- ord$x[tail_ids]
  cutoff <- ord$x[min(tail_ids) - 1] # largest value smaller than tail values
  
  max_tail <- max(draws_tail)
  min_tail <- min(draws_tail)

  if (ndraws_tail >= 5) {
    ord <- sort.int(x, index.return = TRUE)
    if (abs(max_tail - min_tail) < .Machine$double.eps / 100) {
      warning(
        "Can't fit generalized Pareto distribution ",
        "because all tail values are the same.",
        call. = FALSE
      )
      smoothed <- NULL
      k <- NA
    } else {
      # save time not sorting since x already sorted
      fit <- gpdfit(draws_tail - cutoff, sort_x = FALSE)
      k <- fit$k
      sigma <- fit$sigma
      if (is.finite(k)) {
        p <- (seq_len(ndraws_tail) - 0.5) / ndraws_tail
        smoothed <- qgpd(p = p, mu = cutoff, k = k, sigma = sigma)
      }
    }
  } else {
    warning(
      "Can't fit generalized Pareto distribution ",
      "because ndraws_tail is less than 5.",
      call. = FALSE
    )
    smoothed <- NULL
    k <- NA
  }

  # truncate at max of raw draws
  if (!is.null(smoothed)) {
    smoothed[smoothed > max_tail] <- max_tail
    x[ord$ix[tail_ids]] <- smoothed
  }
  
  if (tail == "left") {
    x <- -x
  }
  
  out <- list(x = x, k = k)

  return(out)
}

.pareto_smooth_extra_diags <- function(k, S, ...) {

  min_ss <- ps_min_ss(k)

  khat_threshold <- ps_khat_threshold(S)

  convergence_rate <- ps_convergence_rate(k, S)

  other_diags <- list(
    "min_ss" = min_ss,
    "khat_threshold" = khat_threshold,
    "convergence_rate" = convergence_rate
  )
}

#' Pareto-smoothing minimum sample-size
#'
#' Given Pareto-k computes the minimum sample size for reliable Pareto
#' smoothed estimate (to have small probability of large error)
#' @param k pareto k value
#' @param ... unused
#' @return minimum sample size
ps_min_ss <- function(k, ...) {
  # Eq (11) in PSIS paper
  10^(1 / (1 - max(0, k)))
}

#' Pareto-smoothing k-hat threshold
#'
#' Given sample size S computes khat threshold for reliable Pareto
#' smoothed estimate (to have small probability of large error). See
#' section 3.2.4, equation (13).
#' @param S sample size
#' @param ... unused
#' @return threshold
ps_khat_threshold <- function(S, ...) {
  1 - 1 / log10(S)
}

#' Pareto-smoothing convergence rate
#'
#' Given S and scalar or array of k's, compute the relative
#' convergence rate of PSIS estimate RMSE
#' @param k pareto-k values
#' @param S sample size
#' @param ... unused
#' @return convergence rate
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

ps_tail_length <- function(S, r_eff, ...) {
  # Appendix H in PSIS paper
  ifelse(S > 225, ceiling(3 * sqrt(S / r_eff)), S / 5)
}

#' Pareto-k diagnostic message
#'
#' Given S and scalar and k, form a diagnostic message string
#' @param diags (numeric) named vector of diagnostic values
#' @param ... unused
#' @return diagnostic message
pareto_k_diagmsg <- function(diags, ...) {
  khat <- diags$khat
  min_ss <- diags$min_ss
  khat_threshold <- diags$khat_threshold
  convergence_rate <- diags$convergence_rate
  msg <- NULL
  if (khat > 1) {
    msg <- paste0(msg,'All estimates are unreliable. If the distribution of ratios is bounded,\n',
                  'further draws may improve the estimates, but it is not possible to predict\n',
                  'whether any feasible sample size is sufficient.')
  } else {
    if (khat > khat_threshold) {
      msg <- paste0(msg, 'S is too small, and sample size larger than ', round(min_ss, 0), ' is neeeded for reliable results.\n')
    } else {
      msg <- paste0(msg, 'To halve the RMSE, approximately ', round(2^(2/convergence_rate),1), ' times bigger S is needed.\n')
    }
    if (khat > 0.7) {
      msg <- paste0(msg, 'Bias dominates RMSE, and the variance based MCSE is underestimated.\n')
    }
  }
  message(msg)
}

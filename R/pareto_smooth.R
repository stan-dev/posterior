#' Pareto khat diagnostic
#'
#' Estimate Pareto k value by fitting a Generalized Pareto
#' Distribution to one or two tails of x. This can be used to estimate
#' the number of fractional moments that is useful for convergence
#' diagnostics. For further details see Vehtari et al. (2022).
#'
#' @template args-pareto
#' @template args-methods-dots
#' @template ref-vehtari-paretosmooth-2022
#' @return `khat` estimated Generalized Pareto Distribution shape parameter k
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
                                tail = c("both", "right", "left"),
                                r_eff = NULL,
                                ndraws_tail = NULL,
                                verbose = FALSE,
                                ...) {
  smoothed <- pareto_smooth.default(
    x,
    tail = tail,
    r_eff = r_eff,
    ndraws_tail = ndraws_tail,
    verbose = verbose,
    return_k = TRUE,
    smooth_draws = FALSE,
    ...)
  return(smoothed$diagnostics)
}

#' @rdname pareto_khat
#' @export
pareto_khat.rvar <- function(x, ...) {
  draws_diags <- summarise_rvar_by_element_with_chains(
    x,
    pareto_smooth.default,
    return_k = TRUE,
    smooth_draws = FALSE,
    ...
  )
  dim(draws_diags) <- dim(draws_diags) %||% length(draws_diags)
  margins <- seq_along(dim(draws_diags))

  diags <- list(
    khat = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat)
  )

  diags
}


#' Pareto smoothing diagnostics
#'
#' Compute diagnostics for Pareto smoothing the tail draws of x by
#' replacing tail draws by order statistics of a generalized Pareto
#' distribution fit to the tail(s).
#'
#' @template args-pareto
#' @template args-methods-dots
#' @template ref-vehtari-paretosmooth-2022
#' @return List of Pareto smoothing diagnostics:
#'  * `khat`: estimated Pareto k shape parameter,
#'  * `min_ss`: minimum sample size for reliable Pareto smoothed estimate,
#'  * `khat_threshold`: khat-threshold for reliable Pareto smoothed estimate,
#'  * `convergence_rate`: Pareto smoothed estimate RMSE convergence rate.
#'
#' @details When the fitted Generalized Pareto Distribution is used to
#'   smooth the tail values and these smoothed values are used to
#'   compute expectations, the following diagnostics can give further
#'   information about the reliability of these estimates.
#'
#'  * `min_ss`: Minimum sample size for reliable Pareto smoothed
#' estimate. If the actual sample size is greater than `min_ss`, then
#' Pareto smoothed estimates can be considered reliable. If the actual
#' sample size is lower than `min_ss`, increasing the sample size
#' might result in more reliable estimates. For further details, see
#' Section 3.2.3, Equation 11 in Vehtari et al. (2022).
#'
#'  * `khat_threshold`: Threshold below which k-hat values result in
#' reliable Pareto smoothed estimates. The threshold is lower for
#' smaller effective sample sizes. If k-hat is larger than the
#' threshold, increasing the total sample size may improve reliability
#' of estimates. For further details, see Section 3.2.4, Equation 13
#' in Vehtari et al. (2022).
#'
#'  * `convergence_rate`: Relative convergence rate compared to the
#'  central limit theorem. Applicable only if the actual sample size
#'  is sufficiently large (greater than `min_ss`). The convergence
#'  rate tells the rate at which the variance of an estimate reduces
#'  when the sample size is increased, compared to the central limit
#'  theorem convergence rate. See Appendix B in Vehtari et al. (2022).
#'
#' @examples
#' mu <- extract_variable_matrix(example_draws(), "mu")
#' pareto_diags(mu)
#'
#' d <- as_draws_rvars(example_draws("multi_normal"))
#' pareto_diags(d$Sigma)
#' @export
pareto_diags <- function(x, ...) UseMethod("pareto_diags")


#' @rdname pareto_diags
#' @export
pareto_diags.default <- function(x,
                                  tail = c("both", "right", "left"),
                                  r_eff = NULL,
                                  ndraws_tail = NULL,
                                  verbose = FALSE,
                                  ...) {

  smoothed <- pareto_smooth.default(
    x,
    tail = tail,
    r_eff = r_eff,
    ndraws_tail = ndraws_tail,
    return_k = TRUE,
    extra_diags = TRUE,
    verbose = verbose,
    smooth_draws = FALSE,
    ...)

  return(smoothed$diagnostics)

}


#' @rdname pareto_diags
#' @export
pareto_diags.rvar <- function(x, ...) {
  draws_diags <- summarise_rvar_by_element_with_chains(
    x,
    pareto_smooth.default,
    return_k = TRUE,
    smooth_draws = FALSE,
    extra_diags = TRUE,
    ...
  )

  dim(draws_diags) <- dim(draws_diags) %||% length(draws_diags)
  margins <- seq_along(dim(draws_diags))

  diags <- list(
    khat = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat),
    min_ss = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$min_ss),
    khat_threshold = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat_threshold),
    convergence_rate = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$convergence_rate)
  )

  diags
}



#' Pareto smoothing
#'
#' Smooth the tail draws of x by replacing tail draws by order
#' statistics of a generalized Pareto distribution fit to the
#' tail(s). For further details see Vehtari et al. (2022).
#'
#' @template args-pareto
#' @param return_k (logical) Should the Pareto khat be included in
#'   output? If `TRUE`, output will be a list containing of smoothed
#'   draws and diagnostics. Default is `TRUE`.
#' @param extra_diags (logical) Should extra Pareto khat diagnostics
#'   be included in output? If `TRUE`, `min_ss`, `khat_threshold` and
#'   `convergence_rate` for the estimated k value will be
#'   returned. Default is `FALSE`.
#' @template args-methods-dots
#' @template ref-vehtari-paretosmooth-2022
#' @return Either a vector `x` of smoothed values or a named list
#'   containing the vector `x` and a named list `diagnostics` containing Pareto smoothing
#'   diagnostics:
#' * `khat`: estimated Pareto k shape parameter, and
#'   optionally
#' * `min_ss`: minimum sample size for reliable Pareto
#'   smoothed estimate
#' * `khat_threshold`: khat-threshold for reliable
#'   Pareto smoothed estimates
#' * `convergence_rate`: Relative convergence rate for Pareto smoothed estimates
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
pareto_smooth.rvar <- function(x, return_k = TRUE, extra_diags = FALSE, ...) {

  if (extra_diags) {
    return_k <- TRUE
  }

  draws_diags <- summarise_rvar_by_element_with_chains(x, pareto_smooth.default, return_k = return_k, extra_diags = extra_diags, ...)
  dim(draws_diags) <- dim(draws_diags) %||% length(draws_diags)
  margins <- seq_along(dim(draws_diags))

  if (return_k) {
    if (extra_diags) {

      diags <- list(
        khat = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat),
        min_ss = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$min_ss),
        khat_threshold = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat_threshold),
        convergence_rate = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$convergence_rate)
      )
    } else {
      diags <- list(
        khat = apply(draws_diags, margins, function(x) x[[1]]$diagnostics$khat)
      )
    }
    out <- list(
    x = rvar(apply(draws_diags, margins, function(x) x[[1]]$x), nchains = nchains(x)),
    diagnostics = diags
    )
  } else {
    out <- rvar(apply(draws_diags, margins, function(x) x[[1]]), nchains = nchains(x))
  }
  out
}

#' @rdname pareto_smooth
#' @export
pareto_smooth.default <- function(x,
                                  tail = c("both", "right", "left"),
                                  r_eff = NULL,
                                  ndraws_tail = NULL,
                                  return_k = TRUE,
                                  extra_diags = FALSE,
                                  verbose = FALSE,
                                  ...) {

  checkmate::assert_number(ndraws_tail, null.ok = TRUE)
  checkmate::assert_number(r_eff, null.ok = TRUE)
  checkmate::assert_logical(extra_diags)
  checkmate::assert_logical(return_k)
  checkmate::assert_logical(verbose)

  # check for infinite or na values
  if (should_return_NA(x)) {
    warning_no_call("Input contains infinite or NA values, Pareto smoothing not performed.")
    return(list(x = x, diagnostics = NA_real_))
  }

  tail <- match.arg(tail)
  S <- length(x)

  # automatically calculate relative efficiency
  if (is.null(r_eff)) {
    r_eff <- ess_tail(x) / S
  }

  # automatically calculate tail length
  if (is.null(ndraws_tail)) {
    ndraws_tail <- ps_tail_length(S, r_eff)
  }

  if (tail == "both") {

    if (ndraws_tail > S / 2) {
      warning("Number of tail draws cannot be more than half ",
              "the total number of draws if both tails are fit, ",
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
      tail = "left",
      ...
    )
    left_k <- smoothed$k

    # right tail
    smoothed <-.pareto_smooth_tail(
      x = smoothed$x,
      ndraws_tail = ndraws_tail,
      tail = "right",
      ...
    )
    right_k <- smoothed$k

    k <- max(left_k, right_k)
    x <- smoothed$x
  } else {

    smoothed <- .pareto_smooth_tail(
      x,
      ndraws_tail = ndraws_tail,
      tail = tail,
      ...
    )
    k <- smoothed$k
    x <- smoothed$x
  }

  diags_list <- list(khat = k)

  if (extra_diags) {
    ext_diags <- .pareto_smooth_extra_diags(k, S)
    diags_list <- c(diags_list, ext_diags)
  }

  if (verbose) {
    if (!extra_diags) {
      diags_list <- .pareto_smooth_extra_diags(diags_list$khat, length(x))
    }
    pareto_k_diagmsg(
      diags = diags_list
    )
  }

  if (return_k) {
    out <- list(x = x, diagnostics = diags_list)
  } else {
    out <- x
  }

  return(out)
}

#' Pareto smooth tail
#' internal function to pareto smooth the tail of a vector
#' @noRd
.pareto_smooth_tail <- function(x,
                                ndraws_tail,
                                smooth_draws = TRUE,
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
      warning_no_call(
        "Can't fit generalized Pareto distribution ",
        "because all tail values are the same."
      )
      smoothed <- NULL
      k <- NA
    } else {
      # save time not sorting since x already sorted
      fit <- gpdfit(draws_tail - cutoff, sort_x = FALSE)
      k <- fit$k
      sigma <- fit$sigma
      if (is.finite(k) && smooth_draws) {
        p <- (seq_len(ndraws_tail) - 0.5) / ndraws_tail
        smoothed <- qgeneralized_pareto(p = p, mu = cutoff, k = k, sigma = sigma)
      } else {
        smoothed <- NULL
      }
    }
  } else {
    warning_no_call(
      "Can't fit generalized Pareto distribution ",
      "because ndraws_tail is less than 5."
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

#' Extra pareto-k diagnostics
#'
#' internal function to calculate the extra diagnostics for a given
#' pareto k and sample size S
#' @noRd
.pareto_smooth_extra_diags <- function(k, S, ...) {

  min_ss <- ps_min_ss(k)

  khat_threshold <- ps_khat_threshold(S)

  convergence_rate <- ps_convergence_rate(k, S)

  other_diags <- list(
    min_ss = min_ss,
    khat_threshold = khat_threshold,
    convergence_rate = convergence_rate
  )
}

#' Pareto-smoothing minimum sample-size
#'
#' Given Pareto-k computes the minimum sample size for reliable Pareto
#' smoothed estimate (to have small probability of large error)
#' Equation (11) in PSIS paper
#' @param k pareto k value
#' @param ... unused
#' @return minimum sample size
#' @noRd
ps_min_ss <- function(k, ...) {
  if (k < 1) {
   out <- 10^(1 / (1 - max(0, k)))
  } else {
   out <- Inf
  }
   out
}


#' Pareto-smoothing k-hat threshold
#'
#' Given sample size S computes khat threshold for reliable Pareto
#' smoothed estimate (to have small probability of large error). See
#' section 3.2.4, equation (13).
#' @param S sample size
#' @param ... unused
#' @return threshold
#' @noRd
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
#' @noRd
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

#' Calculate the tail length from S and r_eff
#' Appendix H in PSIS paper
#' @noRd
ps_tail_length <- function(S, r_eff, ...) {
  ifelse(S > 225, ceiling(3 * sqrt(S / r_eff)), S / 5)
}

#' Pareto-k diagnostic message
#'
#' Given S and scalar and k, form a diagnostic message string
#' @param diags (numeric) named vector of diagnostic values
#' @param ... unused
#' @return diagnostic message
#' @noRd
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
      msg <- paste0(msg, 'S is too small, and sample size larger than ', round(min_ss, 0), ' is needed for reliable results.\n')
    } else {
      msg <- paste0(msg, 'To halve the RMSE, approximately ', round(2^(2/convergence_rate),1), ' times bigger S is needed.\n')
    }
    if (khat > 0.7) {
      msg <- paste0(msg, 'Bias dominates RMSE, and the variance based MCSE is underestimated.\n')
    }
  }
  message(msg)
  invisible(diags)
}

#' Pareto khat diagnostic
#'
#' Estimate Pareto k value by fitting a Generalized Pareto
#' Distribution to one or two tails of x. This can be used to estimate
#' the number of fractional moments that is useful for convergence
#' diagnostics. For further details see Vehtari et al. (2024).
#'
#' @family diagnostics
#' @template args-pareto
#' @template args-methods-dots
#' @template ref-vehtari-paretosmooth-2022
#' @template return-conv
#'
#' @seealso [`pareto_diags`] for additional related diagnostics, and
#'   [`pareto_smooth`] for Pareto smoothed draws.
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
                                are_log_weights = FALSE,
                                ...) {
  smoothed <- pareto_smooth.default(
    x,
    tail = tail,
    r_eff = r_eff,
    ndraws_tail = ndraws_tail,
    verbose = verbose,
    return_k = TRUE,
    smooth_draws = FALSE,
    are_log_weights = are_log_weights
  )
  return(smoothed$diagnostics$khat)
}

#' @rdname pareto_khat
#' @export
pareto_khat.rvar <- function(x, ...) {
  draws_diags <- summarise_rvar_by_element_with_chains(
    x,
    pareto_khat.default,
    ...
  )
  dim(draws_diags) <- dim(draws_diags) %||% length(draws_diags)
  margins <- seq_along(dim(draws_diags))

  diags <- list(
    khat = apply(draws_diags, margins, function(x) x[[1]])
  )

  diags$khat
}


#' Pareto smoothing diagnostics
#'
#' Compute diagnostics for Pareto smoothing the tail draws of x by
#' replacing tail draws by order statistics of a generalized Pareto
#' distribution fit to the tail(s).
#'
#' @family diagnostics
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
#' Section 3.2.3, Equation 11 in Vehtari et al. (2024).
#'
#'  * `khat_threshold`: Threshold below which k-hat values result in
#' reliable Pareto smoothed estimates. The threshold is lower for
#' smaller effective sample sizes. If k-hat is larger than the
#' threshold, increasing the total sample size may improve reliability
#' of estimates. For further details, see Section 3.2.4, Equation 13
#' in Vehtari et al. (2024).
#'
#'  * `convergence_rate`: Relative convergence rate compared to the
#'  central limit theorem. Applicable only if the actual sample size
#'  is sufficiently large (greater than `min_ss`). The convergence
#'  rate tells the rate at which the variance of an estimate reduces
#'  when the sample size is increased, compared to the central limit
#'  theorem convergence rate. See Appendix B in Vehtari et al. (2024).
#'
#' @seealso [`pareto_khat`], [`pareto_min_ss`],
#'   [`pareto_khat_threshold`], and [`pareto_convergence_rate`] for
#'   individual diagnostics; and [`pareto_smooth`] for Pareto smoothing
#'   draws.
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
                                 are_log_weights = FALSE,
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
    are_log_weights = are_log_weights,
    ...)

  return(smoothed$diagnostics)

}


#' @rdname pareto_diags
#' @export
pareto_diags.rvar <- function(x, ...) {
  draws_diags <- summarise_rvar_by_element_with_chains(
    x,
    pareto_diags.default,
    ...
  )

  dim(draws_diags) <- dim(draws_diags) %||% length(draws_diags)
  margins <- seq_along(dim(draws_diags))

  diags <- list(
    khat = apply(draws_diags, margins, function(x) x[[1]]$khat),
    min_ss = apply(draws_diags, margins, function(x) x[[1]]$min_ss),
    khat_threshold = apply(draws_diags, margins, function(x) x[[1]]$khat_threshold),
    convergence_rate = apply(draws_diags, margins, function(x) x[[1]]$convergence_rate)
  )

  diags
}



#' Pareto smoothing
#'
#' Smooth the tail draws of x by replacing tail draws by order
#' statistics of a generalized Pareto distribution fit to the
#' tail(s). For further details see Vehtari et al. (2024).
#'
#' @template args-pareto
#' @param return_k (logical) Should the Pareto khat be included in
#'   output? If `TRUE`, output will be a list containing smoothed
#'   draws and diagnostics, otherwise it will be a numeric of the
#'   smoothed draws. Default is `FALSE`.
#' @param extra_diags (logical) Should extra Pareto khat diagnostics
#'   be included in output? If `TRUE`, `min_ss`, `khat_threshold` and
#'   `convergence_rate` for the estimated k value will be
#'   returned. Default is `FALSE`.
#' @template args-methods-dots
#' @template ref-vehtari-paretosmooth-2022
#' @return Either a vector `x` of smoothed values or a named list
#'   containing the vector `x` and a named list `diagnostics`
#'   containing numeric values:
#'
#' * `khat`: estimated Pareto k shape parameter, and optionally
#' * `min_ss`: minimum sample size for reliable Pareto smoothed
#'   estimate
#' * `khat_threshold`: sample size specific khat threshold for
#'   reliable Pareto smoothed estimates
#' * `convergence_rate`: Relative convergence rate for
#'   Pareto smoothed estimates
#'
#' If any of the draws is non-finite, that is, `NA`, `NaN`, `Inf`, or
#' `-Inf`, Pareto smoothing will not be performed, and the original
#' draws will be returned and and diagnostics will be `NA` (numeric).
#'
#' @seealso [`pareto_khat`] for only calculating khat, and
#'   [`pareto_diags`] for additional diagnostics.
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
pareto_smooth.rvar <- function(x, return_k = FALSE, extra_diags = FALSE, ...) {

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
                                  return_k = FALSE,
                                  extra_diags = FALSE,
                                  verbose = TRUE,
                                  are_log_weights = FALSE,
                                  ...) {

  if (!is.null(r_eff)) {
    r_eff <- as_one_numeric(r_eff)
  }
  if (!is.null(ndraws_tail)) {
    ndraws_tail <- as_one_integer(ndraws_tail)
  }
  extra_diags <- as_one_logical(extra_diags)
  return_k <- as_one_logical(return_k)
  verbose <- as_one_logical(verbose)
  are_log_weights <- as_one_logical(are_log_weights)

  if (extra_diags) {
    return_k <- TRUE
  }

  # check for infinite or na values
  if (should_return_NA(x)) {
    warning_no_call("Input contains infinite or NA values, is constant or has constant tail. Fitting of generalized Pareto distribution not performed.")
    return(pareto_diags_na(x, return_k, extra_diags))
  }

  if (are_log_weights) {
    tail <- "right"
  }

  tail <- match.arg(tail)
  ndraws <- length(x)

  # automatically calculate relative efficiency
  if (is.null(r_eff)) {
    r_eff <- ess_tail(x) / ndraws
  }

  # automatically calculate tail length
  if (is.null(ndraws_tail)) {
    ndraws_tail <- ps_tail_length(ndraws, r_eff)
  }

  if (is.na(ndraws_tail)) {
    warning_no_call("Input contains infinite or NA values, is constant, or has constant tail. Fitting of generalized Pareto distribution not performed.")
    return(pareto_diags_na(x, return_k, extra_diags))
  }

  if (tail == "both") {

    if (ndraws_tail > ndraws / 2) {
      warning("Number of tail draws cannot be more than half ",
              "the total number of draws if both tails are fit, ",
              "changing to ", ndraws / 2, ".")
      ndraws_tail <- ndraws / 2
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
      are_log_weights = are_log_weights,
      ...
    )
    left_k <- smoothed$k

    # right tail
    smoothed <-.pareto_smooth_tail(
      x = smoothed$x,
      ndraws_tail = ndraws_tail,
      tail = "right",
      are_log_weights = are_log_weights,
      ...
    )
    right_k <- smoothed$k

    k <- max(left_k, right_k, na.rm = TRUE)
    x <- smoothed$x

  } else {

    smoothed <- .pareto_smooth_tail(
      x,
      ndraws_tail = ndraws_tail,
      tail = tail,
      are_log_weights = are_log_weights,
      ...
    )
    k <- smoothed$k
    x <- smoothed$x
  }

  diags_list <- list(khat = k)

  if (extra_diags) {
    ext_diags <- .pareto_smooth_extra_diags(k, ndraws)
    diags_list <- c(diags_list, ext_diags)
  }

  if (verbose) {
    if (!extra_diags) {
      diags_list <- c(diags_list, .pareto_smooth_extra_diags(diags_list$khat, length(x)))
    }
    pareto_k_diagmsg(
      diags = diags_list,
      are_weights = are_log_weights
    )
  }

  if (return_k) {
    out <- list(x = x, diagnostics = diags_list)
  } else {
    out <- x
  }

  return(out)
}

#' @rdname pareto_diags
#' @export
pareto_khat_threshold <- function(x, ...) {
  UseMethod("pareto_khat_threshold")
}

#' @rdname pareto_diags
#' @export
pareto_khat_threshold.default <- function(x, ...) {
  ps_khat_threshold(length(x))
}

#' @rdname pareto_diags
#' @export
pareto_khat_threshold.rvar <- function(x, ...) {
  ps_khat_threshold(ndraws(x))
}

#' @rdname pareto_diags
#' @export
pareto_min_ss <- function(x, ...) {
  UseMethod("pareto_min_ss")
}

#' @rdname pareto_diags
#' @export
pareto_min_ss.default <- function(x, ...) {
  k <- pareto_khat(x)
  ps_min_ss(k)
}

#' @rdname pareto_diags
#' @export
pareto_min_ss.rvar <- function(x, ...) {
  k <- pareto_khat(x)
  ps_min_ss(k)
}

#' @rdname pareto_diags
#' @export
pareto_convergence_rate <- function(x, ...) {
  UseMethod("pareto_convergence_rate")
}

#' @rdname pareto_diags
#' @export
pareto_convergence_rate.default <- function(x, ...) {
  k <- pareto_khat(x)
  ps_convergence_rate(k, length(x))
}

#' @rdname pareto_diags
#' @export
pareto_convergence_rate.rvar <- function(x, ...) {
  k <- pareto_khat(x)
  ps_convergence_rate(k, ndraws(x))
}


#' Pareto smooth tail
#' internal function to pareto smooth the tail of a vector
#' @noRd
.pareto_smooth_tail <- function(x,
                                ndraws_tail,
                                smooth_draws = TRUE,
                                tail = c("right", "left"),
                                are_log_weights = FALSE,
                                ...
                                ) {

  if (are_log_weights) {
    # shift log values for safe exponentiation
    x <- x - max(x)
  }

  tail <- match.arg(tail)

  ndraws <- length(x)
  tail_ids <- seq(ndraws - ndraws_tail + 1, ndraws)

  if (tail == "left") {
    x <- -x
  }

  ord <- sort.int(x, index.return = TRUE)
  draws_tail <- ord$x[tail_ids]

  if (is_constant(draws_tail)) {

    if (tail == "left") {
      x <- -x
    }

    out <- list(x = x, k = NA)
    return(out)
  }

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
      if (are_log_weights) {
        draws_tail <- exp(draws_tail)
        cutoff <- exp(cutoff)
      }
      fit <- gpdfit(draws_tail - cutoff, sort_x = FALSE, ...)
      k <- fit$k
      sigma <- fit$sigma
      if (is.finite(k) && smooth_draws) {
        p <- (seq_len(ndraws_tail) - 0.5) / ndraws_tail
        smoothed <- qgeneralized_pareto(p = p, mu = cutoff, k = k, sigma = sigma)
        if (are_log_weights) {
          smoothed <- log(smoothed)
        }
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
#' pareto k and number of draws ndraws
#' @noRd
.pareto_smooth_extra_diags <- function(k, ndraws, ...) {

  min_ss <- ps_min_ss(k)

  khat_threshold <- ps_khat_threshold(ndraws)

  convergence_rate <- ps_convergence_rate(k, ndraws)

  other_diags <- list(
    min_ss = min_ss,
    khat_threshold = khat_threshold,
    convergence_rate = convergence_rate
  )
}

#' Pareto-smoothing minimum sample-size
#'
#' Given Pareto-k computes the minimum sample size for reliable Pareto
#' smoothed estimate (to have small probability of large error). See
#' section 3.2.3 in Vehtari et al. (2024). This function is exported
#' to be usable by other packages. For user-facing diagnostic functions, see
#' [`pareto_min_ss`] and [`pareto_diags`].
#' @family helper-functions
#' @param k pareto k value
#' @param ... unused
#' @return minimum sample size
#' @export
ps_min_ss <- function(k, ...) {
  if (k < 1) {
    out <- 10^(1 / (1 - max(0, k)))
  } else {
    out <- Inf
  }
  out
}

#' Pareto k-hat threshold
#'
#' Given number of draws, computes khat threshold for reliable Pareto
#' smoothed estimate (to have small probability of large error). See
#' section 3.2.4, equation (13) of Vehtari et al. (2024). This
#' function is exported to be usable by other packages. For
#' user-facing diagnostic functions, see [`pareto_khat_threshold`] and
#' [`pareto_diags`].
#' @family helper-functions
#' @param ndraws number of draws
#' @param ... unused
#' @return threshold
#' @export
ps_khat_threshold <- function(ndraws, ...) {
  1 - 1 / log10(ndraws)
}

#' Pareto convergence rate
#'
#' Given number of draws and scalar or array of k's, compute the
#' relative convergence rate of PSIS estimate RMSE. See Appendix B of
#' Vehtari et al. (2024). This function is exported to be usable by
#' other packages. For user-facing diagnostic functions, see
#' [`pareto_convergence_rate`] and [`pareto_diags`].
#' @family helper-functions
#' @param k pareto-k values
#' @param ndraws number of draws
#' @param ... unused
#' @return convergence rate
#' @export
ps_convergence_rate <- function(k, ndraws, ...) {
  # allow array of k's
  rate <- numeric(length(k))
  # k<0 bounded distribution
  rate[k < 0] <- 1
  # k>0 non-finite mean
  rate[k > 1] <- 0
  # limit value at k=1/2
  rate[k == 0.5] <- 1 - 1 / log(ndraws)
  # smooth approximation for the rest (see Appendix B of PSIS paper)
  ki <- (k > 0 & k < 1 & k != 0.5)
  kk <- k[ki]
  rate[ki] <- pmax(
    0,
    (2 * (kk - 1) * ndraws^(2 * kk + 1) + (1 - 2 * kk) * ndraws^(2 * kk) + ndraws^2) /
      ((ndraws - 1) * (ndraws - ndraws^(2 * kk)))
  )
  rate
}

#' Pareto tail length
#'
#' Calculate the tail length from number of draws and relative efficiency
#' r_eff. See Appendix H in Vehtari et al. (2024). This function is
#' used internally and is exported to be available for other packages.
#' @family helper-functions
#' @param ndraws number of draws
#' @param r_eff relative efficiency
#' @param ... unused
#' @return tail length
#' @export
ps_tail_length <- function(ndraws, r_eff, ...) {
  ceiling(ifelse(ndraws > 225, 3 * sqrt(ndraws / r_eff), ndraws / 5))
}

#' Pareto-k diagnostic message
#'
#' Given number of draws and k, form a diagnostic message string.
#' @param diags (numeric) named vector of diagnostic values
#' @param are_weights (logical) are the diagnostics for weights
#' @param ... unused
#' @return diagnostic message
#' @noRd
pareto_k_diagmsg <- function(diags, are_weights = FALSE, ...) {
  khat <- diags$khat
  min_ss <- diags$min_ss
  khat_threshold <- diags$khat_threshold
  convergence_rate <- diags$convergence_rate
  msg <- NULL

  if (!are_weights) {

    if (khat > 1) {
      msg <- paste0(msg, " Mean does not exist, making empirical mean estimate of the draws not applicable.")
    } else {
      if (khat > khat_threshold) {
        msg <- paste0(msg, " Sample size is too small, for given Pareto k-hat. Sample size larger than ", round(min_ss, 0), " is needed for reliable results.\n")
      }
      if (khat > 0.7) {
        msg <- paste0(msg, " Bias dominates when k-hat > 0.7, making empirical mean estimate of the Pareto-smoothed draws unreliable.\n")
      }
    }
  } else {
    if (khat > khat_threshold || khat > 0.7) {
        msg <- paste0(msg, " Pareto khat for weights is high (", round(khat, 1) ,"). This indicates a single or few weights dominate.\n", "Inference based on weighted draws will be unreliable.\n")
    }
  }
  message("Pareto k-hat = ", round(khat, 2), ".", msg)
  invisible(diags)
}


pareto_diags_na <- function(x, return_k, extra_diags) {
    if (!return_k) {
      out <- x
    } else if (!extra_diags) {
      out <- list(x = x, diagnostics = list(khat = NA_real_))
    } else {
      out <- list(
        x = x,
        diagnostics = list(
          khat = NA_real_,
          min_ss = NA_real_,
          khat_threshold = NA_real_,
          convergence_rate = NA_real_
        )
      )
    }
    return(out)
}

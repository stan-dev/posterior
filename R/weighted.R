# weighted distribution functions --------------------------------------------

#' Weighted version of [stats::ecdf()].
#' Based on ggdist::weighted_ecdf().
#' @noRd
weighted_ecdf = function(x, weights = NULL) {
  n = length(x)
  if (n < 1) stop("Need at least 1 or more values to calculate an ECDF")

  weights = if (is.null(weights)) rep(1, n) else weights

  #sort only if necessary
  if (is.unsorted(x)) {
    sort_order = order(x)
    x = x[sort_order]
    weights = weights[sort_order]
  }

  # calculate weighted cumulative probabilities
  p = cumsum(weights)
  p = p/p[n]

  approxfun(x, p, yleft = 0, yright = 1, ties = "ordered", method = "constant")
}

#' Weighted version of [stats::quantile()].
#' Based on ggdist::weighted_quantile().
#' @noRd
weighted_quantile = function(x,
  probs = seq(0, 1, 0.25),
  weights = NULL,
  na.rm = FALSE,
  type = 7,
  ...
) {
  weighted_quantile_fun(
    x,
    weights = weights,
    na.rm = na.rm,
    type = type,
    ...
  )(probs)
}

#' @noRd
weighted_quantile_fun = function(x, weights = NULL, na.rm = FALSE, type = 7, ...) {
  na.rm <- as_one_logical(na.rm)
  assert_number(type, lower = 1, upper = 9)

  if (na.rm) {
    keep = !is.na(x) & !is.na(weights)
    x = x[keep]
    weights = weights[keep]
  } else if (anyNA(x)) {
    # quantile itself doesn't handle this case (#110)
    return(function(p) rep(NA_real_, length(p)))
  }

  # determine weights
  weights = weights %||% rep(1, length(x))
  non_zero = weights != 0
  x = x[non_zero]
  weights = weights[non_zero]
  weights = weights / sum(weights)

  # if there is only 0 or 1 x values, we don't need the weighted version (and
  # we couldn't calculate it anyway as we need > 2 points for the interpolation)
  if (length(x) <= 1) {
    return(function(p) quantile(x, p, names = FALSE))
  }

  # sort values if necessary
  if (is.unsorted(x)) {
    x_order = order(x)
    x = x[x_order]
    weights = weights[x_order]
  }

  # calculate the weighted CDF
  F_k = cumsum(weights)

  # generate the function for the approximate inverse CDF
  if (1 <= type && type <= 3) {
    # discontinuous quantiles
    switch(type,
      # type 1
      stepfun(F_k, c(x, x[length(x)]), right = TRUE),
      # type 2
      {
        x_over_2 = c(x, x[length(x)])/2
        inverse_cdf_type2_left = stepfun(F_k, x_over_2, right = FALSE)
        inverse_cdf_type2_right = stepfun(F_k, x_over_2, right = TRUE)
        function(x) inverse_cdf_type2_left(x) + inverse_cdf_type2_right(x)
      },
      # type 3
      stepfun(F_k - weights/2, c(x[[1]], x), right = TRUE)
    )
  } else {
    # Continuous quantiles. These are based on the definition of p_k as described
    # in the documentation of `quantile()`. The trick to re-writing those formulas
    # (which use `n` and `k`) for the weighted case is that `k` = `F_k * n` and
    # `1/n` = `weight_k`. Using these two facts, we can express the formulas for
    # `p_k` without using `n` or `k`, which don't really apply in the weighted case.
    p_k = switch(type - 3,
      # type 4
      F_k,
      # type 5
      F_k - weights/2,
      # type 6
      F_k / (1 + weights),
      # type 7
      (F_k - weights) / (1 - weights),
      # type 8
      (F_k - weights/3) / (1 + weights/3),
      # type 9
      (F_k - weights*3/8) / (1 + weights/4)
    )
    approxfun(p_k, x, rule = 2, ties = "ordered")
  }
}

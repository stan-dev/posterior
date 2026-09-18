# numerically stable version of log(sum(exp(x)))
log_sum_exp <- function(x) {
  x <- as.numeric(x)
  if (length(x) == 0) {
    return(-Inf)
  }
  max <- max(x)
  if (max == -Inf) {
    res <- -Inf
  } else if (max == Inf) {
    res <- Inf
  } else {
    sum <- sum(exp(x - max))
    res <- max + log(sum)
  }
  res
}

# numerically stable version of exp(x) - exp(y)
exp_x_minus_exp_y <- function(x, y) {
  out <- -exp(x) * expm1(y - x)
  # equal infinite inputs give 0 * NaN above; which() drops the NA comparisons
  # that NA or NaN inputs would produce
  out[which(x == y)] <- 0
  out
}

# numerically stable version of log2(1 - x)
log2_one_minus <- function(x) {
  log1p(-x) / log(2)
}

# numerically stable version of log(1 - exp(x)) for x <= 0
log1m_exp <- function(x) {
  log(-expm1(x))
}

# numerically stable version of log(exp(x) / sum(exp(x)))
log_normalize <- function(x) {
  x - log_sum_exp(x)
}

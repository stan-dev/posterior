#' Normalized entropy
#'
#' Normalized entropy, for measuring dispersion in draws from categorical distributions.
#'
#' @param x (multiple options) A vector type to be interpreted as draws from
#' a categorical distribution, such as:
#'  - A [factor]
#'  - A [numeric] (should be [integer] or integer-like)
#'  - An [rvar], [rvar_factor], or [rvar_ordered]
#'
#' @return
#' Shannon entropy of the draws in `x`, normalized to be between 0 (all probability
#' in one category) and 1 (uniform):
#'
#' \deqn{-\frac{\sum_{i = 1}^{n} p_i \log(p_i)}{\log(n)}}
#'
#' If `x` is an [rvar], returns an array of the same shape as `x`, where each
#' cell is the normalized Shannon entropy of the corresponding cell in `x`.
#' @export
entropy <- function(x) {
  UseMethod("entropy")
}
#' @export
entropy.default <- function(x) {
  if (anyNA(x)) return(NA_real_)
  p <- prop.table(table(x))
  n <- length(p)
  if (n == 1) {
    0
  } else {
    p <- p[p > 0]
    -sum(p * log(p)) / log(n)
  }
}
#' @export
entropy.rvar <- function(x) {
  summarise_rvar_by_element(x, entropy)
}

dissent <- function(x) {
  if (anyNA(x)) return(NA_real_)
  if (length(x) == 0) return(0)
  x <- as.numeric(x)
  p <- prop.table(table(x))
  if (length(p) == 1) {
    0
  } else {
    x_i <- as.numeric(names(p))
    -sum(p * log2(1 - abs(x_i - mean(x)) / diff(range(x))))
  }
}

.mode <- function(x) {
  if (anyNA(x)) return(NA_character_)
  x_table <- table(x)
  names(x_table)[which.max(x_table)]
}

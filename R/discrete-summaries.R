#' Normalized entropy
#'
#' Normalized entropy, for measuring dispersion in draws from categorical distributions.
#'
#' @param x (multiple options) A vector to be interpreted as draws from
#' a categorical distribution, such as:
#'  - A [factor]
#'  - A [numeric] (should be [integer] or integer-like)
#'  - An [rvar], [rvar_factor], or [rvar_ordered]
#'
#' @details
#' Calculates the normalized Shannon entropy of the draws in `x`. This value is
#' the entropy of `x` divided by the maximum entropy of a distribution with `n`
#' categories, where `n` is `length(unique(x))` for numeric vectors and
#' `length(levels(x))` for factors:
#'
#' \deqn{-\frac{\sum_{i = 1}^{n} p_i \log(p_i)}{\log(n)}}
#'
#' This scales the output to be between 0 (all probability in one category)
#' and 1 (uniform). This form of normalized entropy is referred to as
#' \eqn{H_\mathrm{REL}} in Wilcox (1967).
#'
#' @returns
#' If `x` is a [factor] or [numeric], returns a length-1 numeric vector with a value
#' between 0 and 1 (inclusive) giving the normalized Shannon entropy of `x`.
#'
#' If `x` is an [rvar], returns an array of the same shape as `x`, where each
#' cell is the normalized Shannon entropy of the draws in the corresponding cell of `x`.
#' @template ref-wilcox-iqv-1967
#' @examples
#' set.seed(1234)
#'
#' levels <- c("a", "b", "c", "d", "e")
#'
#' # a uniform distribution: high normalized entropy
#' x <- factor(
#'   sample(levels, 4000, replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
#'   levels = levels
#' )
#' entropy(x)
#'
#' # a unimodal distribution: low normalized entropy
#' y <- factor(
#'   sample(levels, 4000, replace = TRUE, prob = c(0.95, 0.02, 0.015, 0.01, 0.005)),
#'   levels = levels
#' )
#' entropy(y)
#'
#' # both together, as an rvar
#' xy <- c(rvar(x), rvar(y))
#' xy
#' entropy(xy)
#' @export
entropy <- function(x) {
  UseMethod("entropy")
}
#' @rdname entropy
#' @export
entropy.default <- function(x) {
  if (anyNA(x)) return(NA_real_)
  p <- prop.table(simple_table(x)$count)
  n <- length(p)

  if (n == 1) {
    out <- 0
  } else {
    p <- p[p > 0]
    out <- -sum(p * log(p)) / log(n)
  }
  out
}
#' @rdname entropy
#' @export
entropy.rvar <- function(x) {
  summarise_rvar_by_element(x, entropy)
}


#' Dissention
#'
#' Dissention, for measuring dispersion in draws from ordinal distributions.
#'
#' @param x (multiple options) A vector to be interpreted as draws from
#' an ordinal distribution, such as:
#'  - A [factor]
#'  - A [numeric] (should be [integer] or integer-like)
#'  - An [rvar], [rvar_factor], or [rvar_ordered]
#'
#' @details
#' Calculates Tastle and Wierman's (2007) *dissention* measure:
#'
#' \deqn{-\sum_{i = 1}^{n} p_i \log_2 \left(1 - \frac{|x_i - \mathrm{E}(x)| }{\max(x) - \min(x)} \right)}
#'
#' This ranges from 0 (all probability in one category) through 0.5 (uniform) to
#' 1 (bimodal: all probability split equally between the first and last category).
#'
#' @returns
#' If `x` is a [factor] or [numeric], returns a length-1 numeric vector with a value
#' between 0 and 1 (inclusive) giving the dissention of `x`.
#'
#' If `x` is an [rvar], returns an array of the same shape as `x`, where each
#' cell is the dissention of the draws in the corresponding cell of `x`.
#' @template ref-tastle-wierman-2007
#' @examples
#' set.seed(1234)
#'
#' levels <- c("lowest", "low", "neutral", "high", "highest")
#'
#' # a bimodal distribution: high dissention
#' x <- ordered(
#'   sample(levels, 4000, replace = TRUE, prob = c(0.45, 0.04, 0.02, 0.04, 0.45)),
#'   levels = levels
#' )
#' dissent(x)
#'
#' # a unimodal distribution: low dissention
#' y <- ordered(
#'   sample(levels, 4000, replace = TRUE, prob = c(0.95, 0.02, 0.015, 0.01, 0.005)),
#'   levels = levels
#' )
#' dissent(y)
#'
#' # both together, as an rvar
#' xy <- c(rvar(x), rvar(y))
#' xy
#' dissent(xy)
#' @export
dissent <- function(x) {
  UseMethod("dissent")
}
#' @rdname dissent
#' @export
dissent.default <- function(x) {
  if (anyNA(x)) return(NA_real_)
  if (length(x) == 0) return(0)

  if (is.factor(x)) {
    d <- length(levels(x)) - 1
    x <- as.numeric(x)
  } else {
    d <- diff(range(x))
  }

  tab <- simple_table(x)
  p <- prop.table(tab$count)

  if (length(p) == 1) {
    out <- 0
  } else {
    x_i <- tab$x
    out <- -sum(p * log2(1 - abs(x_i - mean(x)) / d))
  }
  out
}
#' @rdname dissent
#' @export
dissent.rvar <- function(x) {
  summarise_rvar_by_element(x, dissent)
}


#' Modal category
#'
#' Modal category of a vector.
#'
#' @param x (multiple options) A vector to be interpreted as draws from
#' a categorical distribution, such as:
#'  - A [factor]
#'  - A [numeric] (should be [integer] or integer-like)
#'  - An [rvar], [rvar_factor], or [rvar_ordered]
#'
#' @details
#' Finds the modal category (i.e., most frequent value) in `x`. In the case of
#' ties, returns the first tie.
#'
#' @returns
#' If `x` is a [factor] or [numeric], returns a length-1 vector containing
#' the modal value.
#'
#' If `x` is an [rvar], returns an array of the same shape as `x`, where each
#' cell is the modal value of the draws in the corresponding cell of `x`.
#' @examples
#' x <- factor(c("a","b","b","c","d"))
#' modal_category(x)
#'
#' # in the case of ties, the first tie is returned
#' y <- factor(c("a","c","c","d","d"))
#' modal_category(y)
#'
#' # both together, as an rvar
#' xy <- c(rvar(x), rvar(y))
#' xy
#' modal_category(xy)
#' @export
modal_category <- function(x) {
  UseMethod("modal_category")
}
#' @rdname modal_category
#' @export
modal_category.default <- function(x) {
  if (anyNA(x)) return(NA)
  tab <- simple_table(x)
  tab$x[which.max(tab$count)]
}
#' @rdname modal_category
#' @export
modal_category.rvar <- function(x) {
  summarise_rvar_by_element(x, modal_category)
}


# helpers -----------------------------------------------------------------

#' A simpler, faster version of table() that does not mangle values into
#' character strings as names, but preserves them as their original type
#' @param x a vector (numeric, factor, character, etc)
#' @returns a list with two components of the same length
#'  - `x`: unique values from the input `x`
#'  - `count`: count of occurrences of `x`
#' @noRd
simple_table <- function(x) {
  if (is.factor(x)) {
    values <- levels(x)
    x_int <- as.integer(x)
  } else {
    values <- unique(x)
    x_int <- match(x, values)
  }
  list(
    x = values,
    count = tabulate(x_int, nbins = length(values))
  )
}

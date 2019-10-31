#' Example `draws` objects
#'
#' Objects for use in examples.
#'
#' @return A `draws` object.
#'
#' @details
#' Currently the only example is a [`draws_array`] object with 100 iterations
#' from each of 4 Markov chains obtained by fitting the eight schools model
#' described in Gelman et al. (2013) with [Stan](https://mc-stan.org). The
#' variables are:
#' * `mu`: Overall mean of the eight schools
#' * `tau`: Standard deviation between schools
#' * `theta`:  Individual means of each of the eight schools
#'
#' @references
#' Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki Vehtari and
#' Donald B. Rubin (2013). Bayesian Data Analysis, Third Edition. Chapman and
#' Hall/CRC.
#'
#' @examples
#' x <- example_draws()
#' summarise_draws(x)
#'
example_draws <- function() {
  # saved in R/sysdata.rda
  draws_eight_schools
}

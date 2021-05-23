#' Example `draws` objects
#'
#' Objects for use in examples, vignettes, and tests.
#'
#' @param example (string) The name of the example `draws` object. See
#'   **Details** for available options.
#' @return A `draws` object.
#'
#' @details
#' The following example `draws` objects are available.
#'
#' **eight_schools**: A [`draws_array`] object with 100 iterations
#' from each of 4 Markov chains obtained by fitting the eight schools model
#' described in Gelman et al. (2013) with [Stan](https://mc-stan.org). The
#' variables are:
#' * `mu`: Overall mean of the eight schools
#' * `tau`: Standard deviation between schools
#' * `theta`: Individual means of each of the eight schools
#'
#' **multi_normal**: A [`draws_array`] object with 100 iterations from each of
#' the 4 Markov chains obtained by fitting a 3-dimensional multivariate normal
#' model to 100 simulated observations. The variables are:
#' * `mu`: Mean parameter vector of length 3
#' * `Sigma`: Covariance matrix of dimension 3 x 3
#'
#' @note These objects are only intended to be used in demonstrations and tests.
#'   They contain fewer iterations and chains than recommended for performing
#'   actual inference.
#'
#' @references
#' Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki Vehtari and
#' Donald B. Rubin (2013). Bayesian Data Analysis, Third Edition. Chapman and
#' Hall/CRC.
#'
#' @examples
#' draws_eight_schools <- example_draws("eight_schools")
#' summarise_draws(draws_eight_schools)
#'
#' draws_multi_normal <- example_draws("multi_normal")
#' summarise_draws(draws_multi_normal)
#'
#' @export
example_draws <- function(example = "eight_schools") {
  assert_choice(example, example_choices())
  # saved in R/sysdata.rda
  draws_name <- paste0("draws_", example)
  get(draws_name, asNamespace("posterior"))
}

# names of choices for 'example_draws'
example_choices <- function() {
  c("eight_schools", "multi_normal")
}

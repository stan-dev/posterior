#' Draws from the eight schools model
#'
#' @description Posterior draws from the eight schools
#'   model described in Gelman et al. 2013.
#'
#' @aliases eight_schools
#'
#' @format An array of 100 iteration for each of the 4 chains
#'   containing posterior draws of the following variables.
#' \describe{
#'  \item{mu}{Overall mean of the eight schools}
#'  \item{tau}{Standard deviation between schools}
#'  \item{theta]{Indiviudal means for each of the eight schools}
#' }
#'
#' @examples
#' data("draws_eight_schools")
#' draws_eight_schools <- as_draws_array(draws_eight_schools)
#' summarise_draws(draws_eight_schools)
#'
#' @source
#' Andrew Gelman, John B. Carlin, Hal S. Stern, David B. Dunson, Aki Vehtari and
#' Donald B. Rubin (2013). Bayesian Data Analysis, Third Edition. Chapman and
#' Hall/CRC.
"draws_eight_schools"

#' @export
mean_control_variates <- function(x, gradients = NULL, ...) {
  # TODO: make generic
  # TODO: we can add an efficient 'matrix' method as per github
  # but we need this default method to reliably work with summarize_draws
  if (is.null(gradients)) {
    return(mean(x, ...))
  }
  x <- as.vector(x)
  # Getting optimal coefficients for control variates
  coefs <- as.matrix(lm(x ~ gradients)$coefficients[-1])
  # Obtaining the estimate with control variates
  mean(x - gradients %*% coefs)
}

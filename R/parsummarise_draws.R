#' Summaries of `draws` objects with support for parallel computation
#'
#' The `parsummarise_draws()` (and `parsummarize_draws()`) methods are wrappers
#' for `summarise_draws()` that enable parallel computation via package `parallel`.
#'
#' @name draws_summary
#'
#' @param x,object A `draws` object or one coercible to a `draws` object.
#' @param ... Name-value pairs of summary functions.
#'   The name will be the name of the variable in the result unless
#'   the function returns a named vector in which case the latter names
#'   are used. Functions can be passed in all formats supported by
#'   [as_function()][rlang::as_function]. See the 'Examples' section below
#'   for examples.
#' @param .args Optional `list` of additional arguments passed to the summary
#'   functions.
#'
#' @return
#' The `parsummarise_draws()` methods return a [tibble][tibble::tibble] data frame.
#' The first column, `"variable"`, contains the variable names and the remaining
#' columns contain summary statistics and diagnostics.
#'
#' @details
#' `parsummarise_draws()` should always yield output identical to `summarise_draws()`.
#' See `summarise_draws()` for details of default behavior. 
#'
#' @examples
#' x <- example_draws("eight_schools")
#' class(x)
#' str(x)
#'
#' parsummarise_draws(x)
#' parsummarise_draws(x, "mean", "median")
#' parsummarise_draws(x, default_convergence_measures())
#' parsummarise_draws(x, mean, mcse = mcse_mean)
#' parsummarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
#'
#' # illustrate use of '.args'
#' ws <- rexp(ndraws(x))
#' parsummarise_draws(x, weighted.mean, .args = list(w = ws))
#'
NULL

#' @rdname draws_summary
#' @export
parsummarise_draws <- function(x, ..., cores = 1) {
  UseMethod("parsummarise_draws")
}

#' @rdname draws_summary
#' @export
parsummarize_draws <- parsummarise_draws


#' @export
parsummarise_draws.default <- function(x, ..., cores = 1) {
  x <- as_draws(x)
  parsummarise_draws(x, ..., cores = 1)
}

#' @rdname draws_summary
#' @export
parsummarise_draws.draws <- function(x, ..., .args = list(), cores = 1) {
  # some initial wrangling duplicates some of summarise_draws() in order to 
  # consistently handle some edge cases: arrays with no draws, arrays that contain 
  # only reserved variable names, arrays that once broken into chunks yield at least 
  # one chunk with only reserved variable names.
  if (ndraws(x) == 0L) {
    return(empty_draws_summary())
  }
  x <- repair_draws(x)
  x <- as_draws_array(x)
  variables <- variables(x)
  
  if (!length(variables)) {
    warning_no_call(
      "The draws object contained no variables with unreserved names. ",
      "No summaries were computed."
    )
    return(tibble::tibble(character()))
  }
  x <- x[ , , !(dimnames(x)$variable %in% reserved_variables())]
  n_vars <- length(variables)
  chunk_size <- ceiling(n_vars/cores)
  n_chunks <- ceiling(n_vars/chunk_size)
  chunk_list <- vector(length = n_chunks, mode = "list")
  for(i in 1:n_chunks){
    if((chunk_size*(i - 1) + 1) <= n_vars){
      chunk_list[[i]] <- x[ , , (chunk_size*(i - 1) + 1):(min(c(chunk_size*i, n_vars)))]
    }
  }
  summarise_draws2 <- function(x) {summarise_draws(x, ... = ..., .args = .args)}
  if (checkmate::test_os("windows")) {
    cl <- parallel::makePSOCKcluster(cores)
    on.exit(parallel::stopCluster(cl))
    summary_list <- parallel::parLapply(chunk_list, summarise_draws2, cl = cl)
  } else {
    summary_list <- parallel::mclapply(chunk_list, summarise_draws2, mc.cores = cores)
  }
  out <- do.call("rbind", summary_list)
  out
}


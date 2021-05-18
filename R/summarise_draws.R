#' Summaries of `draws` objects
#'
#' The `summarise_draws()` (and `summarize_draws()`) methods provide a quick way
#' to get a table of summary statistics and diagnostics. These methods will
#' convert an object to a `draws` object if it isn't already. For convenience, a
#' [summary()][base::summary] method for `draws` and `rvar` objects are also provided as an
#' alias for `summarise_draws()` if the input object is a `draws` or `rvar`
#' object.
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
#' @param cores Positive number of cores for computing summaries for different variables
#'   in parallel. Coerced to integer if possible, otherwise errors. Defaults to `cores = 1` 
#'   in which case no parallelization is implemented. By default, creates a socket cluster
#'   on Windows and uses forking otherwise.
#'
#' @return
#' The `summarise_draws()` methods return a [tibble][tibble::tibble] data frame.
#' The first column, `"variable"`, contains the variable names and the remaining
#' columns contain summary statistics and diagnostics.
#'
#' @details
#' By default, the following summary functions are used: [mean()], [median()],
#' [sd()], [mad()], [quantile2()], [rhat()], [ess_bulk()], and [ess_tail()].
#' The functions `default_summary_measures()`, `default_convergence_measures()`,
#' and `default_mcse_measures()` return character vectors of names of the
#' default measures included in the package.
#'
#' @examples
#' x <- example_draws("eight_schools")
#' class(x)
#' str(x)
#'
#' summarise_draws(x)
#' summarise_draws(x, "mean", "median")
#' summarise_draws(x, default_convergence_measures())
#' summarise_draws(x, mean, mcse = mcse_mean)
#' summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
#'
#' # illustrate use of '.args'
#' ws <- rexp(ndraws(x))
#' summarise_draws(x, weighted.mean, .args = list(w = ws))
#'
NULL

#' @rdname draws_summary
#' @export
summarise_draws <- function(x, ...) {
  UseMethod("summarise_draws")
}

#' @rdname draws_summary
#' @export
summarize_draws <- summarise_draws


#' @export
summarise_draws.default <- function(x, ...) {
  x <- as_draws(x)
  summarise_draws(x, ...)
}

# Helper functions
create_summary_list <- function(x, v, funs, .args) {
  draws <- drop_dims_or_classes(x[, , v], dims = 3, reset_class = FALSE)
  args <- c(list(draws), .args)
  v_summary <- named_list(names(funs))
  for (m in names(funs)) {
    v_summary[[m]] <- do.call(funs[[m]], args)
  }
  v_summary
}

summarise_draws_helper <- function(x, funs, .args) {
  variables <- variables(x)
  # get length and output names, calculated on the first variable
  out1 <- create_summary_list(x, variables[1], funs, .args)
  the_names <- vector(mode = "list", length = length(funs))
  for (i in seq_along(out1)){
    if (rlang::is_named(out1[[i]])) {
      the_names[[i]] <- names(out1[[i]])
    } else if (length(out1[[i]]) > 1) {
      the_names[[i]] <- paste0(names(out1)[i], ".", c(1:length(out1[[i]])))
    } else {
      the_names[[i]] <- names(out1)[i]
    }
  }
  the_names <- unlist(the_names)
  # Check for naming issues prior do doing lengthy computation
  if ("variable" %in% the_names) {
    stop_no_call("Name 'variable' is reserved in 'summarise_draws'.")
  }
  # Pre-allocate matrix to store output
  out <- matrix(NA, nrow = length(variables), ncol = length(the_names))
  colnames(out) <- the_names
  out[1, ] <- unlist(out1)
  # Do the computation for all remaining variables
  if (length(variables) > 1L) {
    for (v_ind in 2:length(variables)) {
      out_v <- create_summary_list(x, variables[v_ind], funs, .args)
      out[v_ind, ] <- unlist(out_v)
    }
  }
  out <- tibble::as_tibble(out)
  out$variable <- variables
  out <- move_to_start(out, "variable")
  class(out) <- class_draws_summary()
  out
}

#' @rdname draws_summary
#' @export
summarise_draws.draws <- function(x, ..., .args = list(), cores = 1) {
  cores <- as.integer(cores)
  if (is.na(cores) || cores <= 0 || length(cores) != 1) {
    stop_no_call("'cores' must be a positive integer.")
  }
  funs <- as.list(c(...))
  .args <- as.list(.args)
  if (length(funs)) {
    if (is.null(names(funs))) {
      # ensure names are initialized properly
      names(funs) <- rep("", length(funs))
    }
    calls <- substitute(list(...))[-1]
    calls <- ulapply(calls, deparse_pretty)
    for (i in seq_along(funs)) {
      fname <- NULL
      if (is.character(funs[[i]])) {
        fname <- as_one_character(funs[[i]])
      }
      # label unnamed arguments via their calls
      if (!nzchar(names(funs)[i])) {
        if (!is.null(fname)) {
          names(funs)[i] <- fname
        } else {
          names(funs)[i] <- calls[i]
        }
      }
      # get functions passed as stings from the right environments
      if (!is.null(fname)) {
        if (exists(fname, envir = caller_env())) {
          env <- caller_env()
        } else if (fname %in% getNamespaceExports("posterior")) {
          env <- asNamespace("posterior")
        } else {
          stop_no_call("Cannot find function '", fname, "'.")
        }
      }
      funs[[i]] <- rlang::as_function(funs[[i]], env = env)
    }
  } else {
    # default functions
    funs <- list(
      mean = base::mean,
      median = stats::median,
      sd = stats::sd,
      mad = stats::mad,
      quantile = quantile2,
      rhat = rhat,
      ess_bulk = ess_bulk,
      ess_tail = ess_tail
    )
  }

  # it is more efficient to repair and transform objects for all variables
  # at once instead of doing it within the loop for each variable separately
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
  
  if (cores == 1) {
    out <- summarise_draws_helper(x, funs, .args)
  } else {
    x <- x[ , , !(dimnames(x)$variable %in% reserved_variables())]
    n_vars <- length(variables)
    chunk_size <- ceiling(n_vars/cores)
    n_chunks <- ceiling(n_vars/chunk_size)
    chunk_list <- vector(length = n_chunks, mode = "list")
    for (i in 1:n_chunks) {
      if ((chunk_size * (i - 1) + 1) <= n_vars) {
        chunk_list[[i]] <- x[ , , (chunk_size * (i - 1) + 1):(min(c(chunk_size * i, n_vars)))]
      }
    }
    summarise_draws_helper2 <- function(x) {
      summarise_draws_helper(x, funs = funs, .args = .args)
    }
    if (checkmate::test_os("windows")) {
      cl <- parallel::makePSOCKcluster(cores)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterEvalQ(cl = cl, library("posterior"))
      summary_list <- parallel::parLapply(cl = cl, X = chunk_list, fun = summarise_draws_helper2)
    } else {
      summary_list <- parallel::mclapply(X = chunk_list, FUN = summarise_draws_helper2, mc.cores = cores)
    }
    out <- do.call("rbind", summary_list)
  }
  out
}

#' @rdname draws_summary
#' @export
summary.draws <- function(object, ...) {
  summarise_draws(object, ...)
}

#' @rdname draws_summary
#' @export
summarise_draws.rvar <- function(x, ...) {
  .x <- draws_rvars(x = x)
  names(.x) <- deparse_pretty(substitute(x))
  summarise_draws(.x, ...)
}

#' @rdname draws_summary
#' @export
summary.rvar <- function(object, ...) {
  .x <- draws_rvars(x = object)
  names(.x) <- deparse_pretty(substitute(object))
  summarise_draws(.x, ...)
}

#' @rdname draws_summary
#' @export
default_summary_measures <- function() {
  c("mean", "median", "sd", "mad", "quantile2")
}

#' @rdname draws_summary
#' @export
default_convergence_measures <- function() {
  c("rhat", "ess_bulk", "ess_tail")
}

#' @rdname draws_summary
#' @export
default_mcse_measures <- function() {
  c("mcse_mean", "mcse_median", "mcse_sd", "mcse_quantile")
}

class_draws_summary <- function() {
  c("draws_summary", "tbl_df", "tbl", "data.frame")
}

# empty draws_summary object
# @param dimensions names of dimensions to be added as empty columns
empty_draws_summary <- function(dimensions = "variable") {
  assert_character(dimensions, null.ok = TRUE)
  out <- tibble::tibble()
  for (d in dimensions) {
    out[[d]] <- character(0)
  }
  class(out) <- class_draws_summary()
  out
}

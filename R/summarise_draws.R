#' Summaries of `draws` objects
#'
#' The `summarise_draws()` (and `summarize_draws()`) methods provide a quick way
#' to get a table of summary statistics and diagnostics. These methods will
#' convert an object to a `draws` object if it isn't already. For convenience, a
#' [summary()][base::summary] method for `draws` and `rvar` objects are also
#' provided as an alias for `summarise_draws()` if the input object is a `draws`
#' or `rvar` object.
#'
#' @name draws_summary
#'
#' @param .x,object (draws) A `draws` object or one coercible to a `draws` object.
#' @param ... Name-value pairs of summary or [diagnostic][diagnostics]
#'   functions. The provided names will be used as the names of the columns in
#'   the result *unless* the function returns a named vector, in which case the
#'   latter names are used. The functions can be specified in any format
#'   supported by [as_function()][rlang::as_function]. See **Examples**.
#' @param .args (named list) Optional arguments passed to the summary functions.
#' @param .cores (positive integer) The number of cores to use for computing
#'   summaries for different variables in parallel. Coerced to integer if
#'   possible, otherwise errors. The default is `.cores = 1`, in which case no
#'   parallelization is implemented. By default, a socket cluster is used on
#'   Windows and forks otherwise.
#'
#' @return
#' The `summarise_draws()` methods return a [tibble][tibble::tibble] data frame.
#' The first column (`"variable"`) contains the variable names and the remaining
#' columns contain summary statistics and diagnostics.
#'
#' The functions `default_summary_measures()`, `default_convergence_measures()`,
#' and `default_mcse_measures()` return character vectors of names of the
#' default measures.
#'
#' @details
#' The default summary functions used are the ones specified by
#' `default_summary_measures()` and `default_convergence_measures()`:
#'
#' `default_summary_measures()`
#' * [mean()]
#' * [median()]
#' * [sd()]
#' * [mad()]
#' * [quantile2()]
#'
#' `default_convergence_measures()`
#' * [rhat()]
#' * [ess_bulk()]
#' * [ess_tail()]
#'
#' The `var()` function should not be used to compute variances due
#' to its inconsistent behavior with matrices. Instead, please use
#' `distributional::variance()`.
#'
#' @seealso [`diagnostics`] for a list of available diagnostics and links to
#'   their individual help pages.
#'
#' @examples
#' x <- example_draws("eight_schools")
#' class(x)
#' str(x)
#'
#' summarise_draws(x)
#' summarise_draws(x, "mean", "median")
#' summarise_draws(x, mean, mcse = mcse_mean)
#' summarise_draws(x, ~quantile(.x, probs = c(0.4, 0.6)))
#'
#' # using default_*_meaures()
#' summarise_draws(x, default_summary_measures())
#' summarise_draws(x, default_convergence_measures())
#' summarise_draws(x, default_mcse_measures())
#'
#' # compute variance of variables
#' summarise_draws(x, var = distributional::variance)
#'
#' # illustrate use of '.args'
#' ws <- rexp(ndraws(x))
#' summarise_draws(x, weighted.mean, .args = list(w = ws))
#'
NULL

#' @rdname draws_summary
#' @export
summarise_draws <- function(.x, ...) {
  UseMethod("summarise_draws")
}

#' @rdname draws_summary
#' @export
summarize_draws <- summarise_draws


#' @export
summarise_draws.default <- function(.x, ...) {
  .x <- as_draws(.x)
  summarise_draws(.x, ...)
}

#' @rdname draws_summary
#' @export
summarise_draws.draws <- function(.x, ..., .args = list(), .cores = 1) {
  if (ndraws(.x) == 0L) {
    return(empty_draws_summary())
  }

  .cores <- as_one_integer(.cores)
  if (.cores <= 0) {
    stop_no_call("'.cores' must be a positive integer.")
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
      # get functions passed as strings from the right environments
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
  .x <- repair_draws(.x)
  .x <- as_draws_array(.x)
  variables_x <- variables(.x)

  if (!length(variables_x)) {
    warning_no_call(
      "The draws object contained no variables with unreserved names. ",
      "No summaries were computed."
    )
    return(tibble::tibble(character()))
  }

  if (.cores == 1) {
    out <- summarise_draws_helper(.x, funs, .args)
  } else {
    .x <- .x[, , variables_x]
    n_vars <- length(variables_x)
    chunk_size <- ceiling(n_vars / .cores)
    n_chunks <- ceiling(n_vars / chunk_size)
    chunk_list <- vector(length = n_chunks, mode = "list")
    for (i in seq_len(n_chunks)) {
      if ((chunk_size * (i - 1) + 1) <= n_vars) {
        chunk <- (chunk_size * (i - 1) + 1):(min(c(chunk_size * i, n_vars)))
        chunk_list[[i]] <- .x[, , chunk]
      }
    }
    if (checkmate::test_os("windows")) {
      cl <- parallel::makePSOCKcluster(.cores)
      on.exit(parallel::stopCluster(cl))
      # exporting all these functions seems to be required to
      # pass GitHub actions checks on Windows
      parallel::clusterExport(
        cl,
        varlist = package_function_names("posterior"),
        envir = as.environment(asNamespace("posterior"))
      )
      parallel::clusterExport(
        cl,
        varlist = package_function_names("checkmate"),
        envir = as.environment(asNamespace("checkmate"))
      )
      parallel::clusterExport(
        cl,
        varlist = package_function_names("rlang"),
        envir = as.environment(asNamespace("rlang"))
      )
      summary_list <- parallel::parLapply(
        cl,
        X = chunk_list,
        fun = summarise_draws_helper,
        funs = funs,
        .args = .args
      )
    } else {
      summary_list <- parallel::mclapply(
        X = chunk_list,
        FUN = summarise_draws_helper,
        mc.cores = .cores,
        funs = funs,
        .args = .args
      )
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
summarise_draws.rvar <- function(.x, ...) {
  x <- draws_rvars(x = .x)
  names(x) <- deparse_pretty(substitute(.x))
  summarise_draws(x, ...)
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
  variables_x <- variables(x)
  # get length and output names, calculated on the first variable
  out_1 <- create_summary_list(x, variables_x[1], funs, .args)
  the_names <- vector(mode = "list", length = length(funs))
  for (i in seq_along(out_1)){
    if (rlang::is_named(out_1[[i]])) {
      the_names[[i]] <- names(out_1[[i]])
    } else if (length(out_1[[i]]) > 1) {
      the_names[[i]] <- paste0(names(out_1)[i], ".", c(1:length(out_1[[i]])))
    } else {
      the_names[[i]] <- names(out_1)[i]
    }
  }
  the_names <- unlist(the_names)
  # Check for naming issues prior do doing lengthy computation
  if ("variable" %in% the_names) {
    stop_no_call("Name 'variable' is reserved in 'summarise_draws'.")
  }
  # Pre-allocate matrix to store output
  out <- matrix(NA, nrow = length(variables_x), ncol = length(the_names))
  colnames(out) <- the_names
  out[1, ] <- unlist(out_1)
  # Do the computation for all remaining variables
  if (length(variables_x) > 1L) {
    for (v_ind in 2:length(variables_x)) {
      out_v <- create_summary_list(x, variables_x[v_ind], funs, .args)
      out[v_ind, ] <- unlist(out_v)
    }
  }
  out <- tibble::as_tibble(out)
  out$variable <- variables_x
  out <- move_to_start(out, "variable")
  class(out) <- class_draws_summary()
  out
}

#' Roll up `draws_summary` objects by collapsing summaries of non-scalar parameters.
#'
#' Roll up summaries of draws (e.g. as returned by [summarise_draws()]); that
#' is, summarise the summaries. By default, summaries of all variables containing
#' indices (e.g. `"x[1]"`) are rolled up, but the `variable` parameter can be
#' used to roll up specific variables only.
#'
#' @param .x (multiple options) The object containing summaries to roll up. One of:
#'  - a [`draws_summary`] object such as produced by [summarise_draws()].
#'  - a `data.frame` with a `"variable"` column giving the names of variables,
#'    where all other columns are numeric summaries of those variables.
#'  - an object with a [summarise_draws()] method, such as a [`draws`] object,
#'    in which case [summarise_draws()] will be called on `.x` and the result
#'    will be rolled up.
#'  - a [`rollup_summary`] object such as produced by `rollup_summary()`, in
#'    which case variables that have not been rolled up yet may be rolled up.
#' @param ... (multiple options) arguments where the name of each argument is a
#' summary measure (i.e. column) in `.x` and the value is the rollup functions
#' to apply to that summary measure, specified as one of:
#'  - bare name of a function
#'  - a character vector of function names (optionally named).
#'  - a function formula, as accepted by [rlang::as_function()].
#'  - a named list of any of the above.
#'
#' Unnamed arguments in `...` specify default rollup functions to apply to all
#' summary measures that do not have specific rollup functions given in `...` or
#' `.funs`.
#' @param variable (character vector) base names (without indices) of variables
#' to roll up. If `NULL` (the default), all variables with indices in their names
#' (e.g. `"x[1,2]"`) will be rolled up.
#' @param .funs (list) named list where names are summary measures in `.x`
#' and values are the default rollup functions to apply to those summary
#' measures, unless overridden by `...`. As in `...`, unnamed elements of this
#' list give default rollup functions to apply to summary measures that do not
#' have specific rollup functions given in `...` or `.funs`.
#' @details
#' If called without specifying additional rollup functions in `...`,
#' `rollup_summary()` will apply the default rollup functions as determined by
#' `.funs` to the columns in `.x` (or, if `.x` is not a `data.frame`, to the
#' result of `summarise_draws(.x)`).
#'
#' The default value of `.funs` provides several default rollup functions
#' that will be applied to specific summary measures, unless this is overridden
#' by entries in `...`. For example, `ess_bulk` has the default
#' rollup function `"min"` instead of `c("min", "max")`, as the minimum
#' effective sample size is likely of more interest than the maximum.
#' `default_rollups()` gives the complete list of default rollup functions.
#'
#' Calls to `rollup_summary()` can be chained, in which case subsequent
#' rollups will be applied only to variables that have not already been
#' rolled up (i.e. the `"unrolled"` element; see the description of
#' `rollup_summary` objects below). This makes it possible to provide different
#' rollup functions for different variables by calling `rollup_summary()`
#' multiple times with different values of the `variable` parameter.
#' @returns
#' A `rollup_summary` object, which is a named list of [`draws_summary`] objects:
#'  - `"unrolled"`: a [`draws_summary`] of the variables that were not rolled up.
#'  - `"rolled"`: a [`draws_summary`] of the rolled-up variables. The second
#'    column of this data frame, `"dim"`, gives the lengths of the dimensions
#'    of each rolled up variable as a comma-separated character vector. The
#'    remaining columns give the rollups of each summary measure; e.g. if `x`
#'    contained a summary measure `"mean"` and it was rolled up using the `"min"`
#'    and `"max"` functions (the default), the output will have a `"mean_min"`
#'    and `"mean_max"` column.
#' @examples
#' x <- example_draws()
#'
#' # default summaries show a row for every element in array-like variables
#' summarise_draws(x)
#'
#' # you can roll up summaries of array-like variables by rolling up draws
#' # objects directly; this will apply the default options of summarise_draws()
#' rollup_summary(x)
#'
#' # or summarise draws objects first to pick the desired summary measures
#' # (note that ess_bulk is only rolled up using min by default; see the
#' # .funs parameter)
#' ds <- summarise_draws(x, "mean", "sd", "ess_bulk")
#' rollup_summary(ds)
#'
#' # rollups work on variables of any dimension
#' x <- example_draws(example = "multi_normal")
#' rollup_summary(x)
#'
#' # you can roll up only some variables
#' rollup_summary(x, variable = "Sigma")
#'
#' # you can specify the rollup functions to apply to all summaries by passing
#' # unnamed parameters ...
#' rollup_summary(x, "mean", "min")
#'
#' # ... or use names to specify rollup functions for specific summaries
#' rollup_summary(x, mean = "sd", median = "min")
#'
#' # you can pass parameters to rollup functions using anonymous functions
#' x2 <- draws_rvars(x = c(rvar_rng(rnorm, 5), NA))
#' rollup_summary(x2, list(min = function(x) min(x, na.rm = TRUE)))
#'
#' @examplesIf getRversion() >= "4.1"
#' # rollups can be chained to provide different rollup functions to
#' # different variables
#' x |>
#'   summarise_draws("mean", "sd") |>
#'   rollup_summary(variable = "mu", sd = "min") |>
#'   rollup_summary(variable = "Sigma", sd = "max")
#' @export
rollup_summary <- function(.x, ...) {
  UseMethod("rollup_summary")
}

#' @rdname rollup_summary
#' @export
rollup_summary.default <- function(.x, ...) {
  rollup_summary(summarise_draws(.x), ...)
}

#' @rdname rollup_summary
#' @export
rollup_summary.draws <- function(.x, ...) {
  rollup_summary(summarise_draws(.x), ...)
}

#' @rdname rollup_summary
#' @export
rollup_summary.data.frame <- function (
  .x,
  ...,
  variable = NULL,
  .funs = default_rollups()
) {
  assert_multi_class(.x$variable, c("character", "factor"))
  assert_character(variable, null.ok = TRUE)
  assert_list(.funs, null.ok = TRUE)

  rollup_funs <- lapply(rlang::enquos0(...), create_function_list)
  default_rollup_funs <- lapply(.funs, create_function_list)

  is_unnamed <- rlang::names2(rollup_funs) == ""
  if (any(is_unnamed)) {
    # user provided unnamed functions in dots, use these for summary measures
    # that otherwise don't have a rollup function specified
    unspecified_rollup_funs <- do.call(c, rollup_funs[is_unnamed])
    rollup_funs <- rollup_funs[!is_unnamed]
  } else {
    # use the default unspecified rollup funs
    is_unnamed <- rlang::names2(default_rollup_funs) == ""
    unspecified_rollup_funs <- do.call(c, default_rollup_funs[is_unnamed])
    default_rollup_funs <- default_rollup_funs[!is_unnamed]
  }

  # apply the measure-specific default rollup functions to any columns not
  # overridden by the user
  missing_default_funs <- setdiff(names(default_rollup_funs), names(rollup_funs))
  rollup_funs[missing_default_funs] <- default_rollup_funs[missing_default_funs]

  # apply the generic default rollup functions to any remaining unspecified columns
  rollup_funs[setdiff(names(.x), names(rollup_funs))] <- list(unspecified_rollup_funs)

  # determine the variables to roll up
  vars <- split_variable_names(.x$variable)
  if (is.null(variable)) {
    rollup_rows <- nzchar(vars$indices)
  } else {
    rollup_rows <- vars$base_name %in% variable
  }
  variable_col <- which(names(.x) == "variable")
  vars <- vars[rollup_rows, ]

  # split the input df by variable base name and roll up the summaries
  var_groups <- vctrs::vec_split(cbind(vars, .x[rollup_rows, -variable_col, drop = FALSE]), vars$base_name)
  rolled_up_vars <- lapply(var_groups$val, function(x) {
    indices <- split_indices_to_df(x$indices)
    rolled_up_cols <- do.call(cbind, lapply(seq_along(x)[c(-1,-2)], function(col_i) {
      col <- x[[col_i]]
      col_name <- names(x)[[col_i]]
      rolled_up_col <- lapply(rollup_funs[[col_name]], function(f) f(col))
      names(rolled_up_col) <- sprintf("%s_%s", col_name, names(rolled_up_col))
      vctrs::new_data_frame(rolled_up_col, n = 1L)
    }))
    cbind(
      variable = x$base_name[[1]],
      dim = paste0(lengths(lapply(indices, unique)), collapse = ","),
      rolled_up_cols,
      stringsAsFactors = FALSE
    )
  })

  new_rollup_summary(
    unrolled = .x[!rollup_rows, , drop = FALSE],
    rolled = do.call(rbind, rolled_up_vars)
  )
}

#' @rdname rollup_summary
#' @export
rollup_summary.rollup_summary <- function (.x, ...) {
  out <- rollup_summary(.x$unrolled, ...)
  new_rollup_summary(
    unrolled = out$unrolled,
    rolled = vctrs::vec_rbind(.x$rolled, out$rolled)
  )
}

new_rollup_summary <- function(unrolled, rolled) {
  assert_data_frame(unrolled)
  if (!inherits(unrolled, "draws_summary")) class(unrolled) <- class_draws_summary()
  assert_data_frame(rolled)
  if (!inherits(rolled, "draws_summary")) class(rolled) <- class_draws_summary()

  structure(
    list(unrolled = unrolled, rolled = rolled),
    class = class_rollup_summary()
  )
}

class_rollup_summary <- function() {
  c("rollup_summary", "list")
}

#' @export
print.rollup_summary <- function(x, ..., color = TRUE) {
  color <- as_one_logical(color)
  if (color) {
    subtle <- pillar::style_subtle
  } else {
    subtle <- identity
  }

  cat("<rollup_summary>:\n\n")
  if (NROW(x$unrolled) > 0) {
    cat("$unrolled", subtle("(variables that have not been rolled up):"), "\n")
    print(x$unrolled, ...)
    cat("\n")
  }
  if (NROW(x$rolled) > 0) {
    cat("$rolled", subtle("(variables that have been rolled up):"), "\n")
    print(x$rolled, ...)
    cat("\n")
  }
  invisible(x)
}

#' @rdname rollup_summary
#' @export
default_rollups <- function() {
  list(
    c("min", "max"),
    ess_basic = "min",
    ess_bulk = "min",
    ess_mean = "min",
    ess_median = "min",
    ess_quantile = "min",
    ess_sd = "min",
    ess_tail = "min",
    rhat = "max",
    rhat_basic = "max",
    rhat_nested = "max"
  )
}

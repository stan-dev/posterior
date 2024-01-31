#' "Roll up" `draws_summary` objects by collapsing over non-scalar parameters.
#'
#' "Rolls up" summaries of draws (e.g. as returned by [summarise_draws()]).
#' By default, all variables containing indices (e.g. `"x[1]"`) are rolled up,
#' but the `.variable` parameter can be used to roll up specific variables only.
#'
#' @param .x a `draws_summary` object, a [`draws`] object, a `data.frame`, or an
#' object with a [summarise_draws()] method.
#' @param .variable (character vector) base names (without indices) of variables
#' to roll up. If `NULL` (the default), all variables with indices in their names
#' will be rolled up.
#' @param ... a named arguments where each name is a summary measure (i.e. column)
#' in `.x` and the value is a character vector of function names or a named list
#' of functions giving the rollup functions to apply to the corresponding summary
#' measure.
#' @param .default (list) named list where names are summary measures in `.x`
#' and values are the default rollup functions to apply to those summary
#' measures unless overridden by `...`.
#' @param .unspecified (character vector or list) default rollup functions to
#' apply to any summary measure (column) in `.x` that does not have its own
#' specific rollup functions given in `...` or `.default`.
#' @details
#' If called without specifying additional rollup functions in `...`,
#' `rollup_summary()` will apply the functions provided in `.default` and
#' `.unspecified` to the columns in `.x` (or, if `.x` is not a `data.frame`,
#' to the result of `summarise_draws(.x)`).
#'
#' In addition to the defaults for all columns in `.unspecified`, several
#' summary measures have specific default rollup functions associated with them
#' that will be applied unless this is overridden by entries in `...`. For
#' example, `ess_bulk` has the default rollup function `"min"` instead of
#' `c("min", "max")`. `default_rollups()` gives the complete list of default
#' rollup functions.
#'
#' Calls to `rollup_summary()` can be chained, in which case subsequent
#' rollups will be applied only to variables that have not already been
#' rolled up. This makes it possible to provide different rollup functions
#' for different variables by combining chaining with the use of the
#' `.variable` parameter.
#' @returns
#' A named list of [`draws_summary`] objects; i.e. subclasses of [`tibble`],
#' with the following elements:
#'  - `"unrolled"`: a [`draws_summary`] of the variables that were not rolled up.
#'  - `"rolled"`: a [`draws_summary`] of the rolled-up variables. The second
#'    column of this data frame, `"dims"`, gives the lengths of the dimensions
#'    of each rolled up variable as a comma-separated string. The remaining
#'    columns give each roll up of each summary measure; e.g. if `x` contained
#'    a summary measure `"mean"` and it was rolled up using the `"min"` and
#'    `"max"` functions (the default), the output will have a `"mean_min"` and
#'    `"mean_max"` column.
#' @examples
#' x <- example_draws()
#'
#' # default summaries show a row for every element in array-like variables
#' summarise_draws(x)
#'
#' # you can roll up summaries of array-like variables by rolling up draws
#' # objects directly
#' rollup_summary(x)
#'
#' # or summarise draws objects first to pick the desired summary measures
#' ds <- summarise_draws(x, "mean", "sd")
#' rollup_summary(ds)
#'
#' # rollups work on variables of any dimension
#' x <- example_draws(example = "multi_normal")
#' rollup_summary(x)
#'
#' # you can roll up only some variables
#' rollup_summary(x, .variable = "Sigma")
#'
#' # you can also specify the rollup functions to apply to each function
#' rollup_summary(x, "Sigma", mean = "mean", median = "min")
#'
#' # to apply a particular function or functions to all summaries, pass them
#' # to .unspecified and set .default to NULL:
#' rollup_summary(x, .unspecified = "median", .default = NULL)
#'
#' @examplesIf getRversion() > "4.1"
#' # rollups can be chained to provide different rollup functions to
#' # different variables
#' x |>
#'   summarise_draws("mean", "sd") |>
#'   rollup_summary("mu", sd = "min") |>
#'   rollup_summary("Sigma", sd = "max")
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
rollup_summary.data.frame <- function (
  .x,
  .variable = NULL,
  ...,
  .default = default_rollups(),
  .unspecified = c("min", "max")
) {
  funs <- list(...)

  # apply the measure-specific default rollup functions to any columns not
  # overridden by the user
  missing_default_funs <- setdiff(names(.default), names(funs))
  funs[missing_default_funs] <- .default[missing_default_funs]

  # apply the generic default rollup functions to any remaining unspecified columns
  funs[setdiff(names(.x), names(funs))] <- list(.unspecified)

  # turn the function specifications into named lists of functions
  funs <- lapply(funs, function(fun_list) inject(create_function_list(!!!fun_list)))

  # determine the variables to roll up
  vars <- split_variable_names(.x$variable)
  if (is.null(.variable)) {
    rollup_rows <- nzchar(vars$indices)
  } else {
    rollup_rows <- vars$base_name %in% .variable
  }
  variable_col <- which(names(.x) == "variable")
  vars <- vars[rollup_rows, ]

  # split the input df by variable base name and roll up the summaries
  var_groups <- vctrs::vec_split(cbind(vars, .x[rollup_rows, -variable_col]), vars$base_name)
  rolled_up_vars <- lapply(var_groups$val, function(x) {
    indices <- split_indices_to_df(x$indices)
    rolled_up_cols <- do.call(cbind, lapply(seq_along(x)[c(-1,-2)], function(col_i) {
      col <- x[[col_i]]
      col_name <- names(x)[[col_i]]
      rolled_up_col <- lapply(funs[[col_name]], function(f) f(col))
      names(rolled_up_col) <- paste0(col_name, "_", names(rolled_up_col))
      vctrs::new_data_frame(rolled_up_col, n = 1L)
    }))
    cbind(
      variable = x$base_name[[1]],
      dims = paste0(lengths(lapply(indices, unique)), collapse = ","),
      rolled_up_cols
    )
  })

  new_rollup_summary(
    unrolled = .x[!rollup_rows, ],
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

#' Print or format a random variable
#'
#' Printing and formatting methods for [`rvar`]s.
#'
#' @encoding UTF-8
#' @param x,object (rvar) The [`rvar`] to print.
#' @template args-print-digits
#' @template args-print-summary
#' @template args-print-dots
#' @param color (logical) Whether or not to use color when formatting the
#'   output. If `TRUE`, the [pillar::style_num()] functions may be used to
#'   produce strings containing control sequences to produce colored output on
#'   the terminal.
#' @param width The maxmimum width used to print out lists of factor levels
#'   for [`rvar_factor`]s. See [format()].
#' @param vec.len (nonnegative integer) How many 'first few' elements are
#'   displayed of each vector. If `NULL`, defaults to
#'   `getOption("str")$vec.len`, which defaults to 4.
#' @param indent.str (string) The indentation string to use.
#' @param nest.lev (nonnegative integer) Current nesting level in the recursive
#'   calls to `str()`.
#' @param give.attr (logical) If `TRUE` (default), show attributes as sub
#'   structures.
#'
#' @details
#' `print()` and `str()` print out [`rvar`] objects by summarizing each element
#' in the random variable with either its mean±sd or median±mad, depending on
#' the value of `summary`. Both functions use the `format()` implementation for
#' [`rvar`] objects under the hood, which returns a character vector in the
#' mean±sd or median±mad form.
#'
#' @return
#' For `print()`, an invisible version of the input object.
#'
#' For `str()`, nothing; i.e. `invisible(NULL)`.
#'
#' For `format()`, a character vector of the same dimensions as `x` where each
#' entry is of the form `"mean±sd"` or `"median±mad"`, depending on the value
#' of `summary`.
#'
#' @template ref-tastle-wierman-2007
#' @examples
#'
#' set.seed(5678)
#' x = rbind(
#'   cbind(rvar(rnorm(1000, 1)), rvar(rnorm(1000, 2))),
#'   cbind(rvar(rnorm(1000, 3)), rvar(rnorm(1000, 4)))
#' )
#'
#' print(x)
#' print(x, summary = "median_mad")
#'
#' str(x)
#'
#' format(x)
#'
#' @importFrom utils str lsf.str
#' @export
print.rvar <- function(x, ..., summary = NULL, digits = NULL, color = TRUE, width = getOption("width")) {
  digits <- digits %||% getOption("posterior.digits", 2)
  # \u00b1 = plus/minus sign
  summary_functions <- get_summary_functions(draws_of(x), summary)
  plus_minus <- summary_functions[[1]] != "modal_category"
  summary_string <- if (plus_minus) {
    paste0(paste(names(summary_functions), collapse = " \u00b1 "), ":")
  } else {
    paste0(paste(names(summary_functions), collapse = " <"), ">:")
  }
  if (color) {
    summary_string <- pillar::style_subtle(summary_string)
  }
  cat0(rvar_type_full(x), " ", summary_string, "\n")

  x_string <- format(x, summary = summary, digits = digits, color = FALSE, pad_right = " ")
  if (length(x_string) == 0) {
    cat0(rvar_class(x), "()\n")
  } else {
    print(x_string, quote = FALSE)
  }

  if (is_rvar_factor(x)) {
    cat0(format_levels(levels(x), is_rvar_ordered(x), width = width), "\n")
  }

  invisible(x)
}

#' @rdname print.rvar
#' @export
format.rvar <- function(x, ..., summary = NULL, digits = NULL, color = FALSE) {
  digits <- digits %||% getOption("posterior.digits", 2)
  format_rvar_draws(draws_of(x), ..., summary = summary, digits = digits, color = color)
}

#' @rdname print.rvar
#' @export
str.rvar <- function(
  object, ..., summary = NULL, vec.len = NULL,
  indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
  nest.lev = 0, give.attr = TRUE
) {

  str_next <- function(x, ...) {
    str(x, ...,
      summary = summary,
      vec.len = vec.len,
      indent.str = paste(indent.str, ".."),
      nest.lev = nest.lev + 1,
      give.attr = give.attr
    )
  }

  # HEADER
  .draws <- draws_of(object)
  vec.len <- vec.len %||% getOption("str")$vec.len %||% 4

  # flatten all the non-draws dimensions for display
  .dim <- dim(.draws)
  dim(.draws) <- c(.dim[1], prod(.dim[-1]))

  if (dim(.draws)[[2]] > vec.len) {
    .draws <- .draws[, 1:vec.len, drop = FALSE]
    ellipsis <- " ..."
  } else {
    ellipsis <- ""
  }

  cat0(" ", rvar_type_full(object), "  ",
    paste(format_rvar_draws(.draws, summary = summary, trim = TRUE), collapse = "  "),
    ellipsis, "\n"
  )

  # LEVELS
  # for factor-like rvars
  if (is_rvar_factor(object)) {
    cat0(indent.str, "- ", format_levels(levels(object), is_rvar_ordered(object), max_level = vec.len), "\n")
  }

  # ATTRIBUTES
  # we have to be a bit clever about this to hide internal structure we don't
  # want people messing with + the fact that some attributes (like dimnames)
  # are actually attributes of the draws and not the base object.
  if (give.attr) {
    .dimnames <- dimnames(object)
    if (!all(sapply(.dimnames, is.null))) {
      # only show dimnames if they aren't all NULL
      cat0(indent.str, paste0('- dimnames(*)='))
      str_next(.dimnames, ...)
    }

    str_attr <- function(a, base, exclude) {
      a_names <- names(a)
      for (i in seq_along(a)) {
        if (!a_names[[i]] %in% exclude) {
          cat0(indent.str, paste0('- attr(', base, ', "', a_names[[i]], '")='))
          str_next(a[[i]], ...)
        }
      }
    }
    str_attr(attributes(draws_of(object)), "draws_of(*)", c("names", "dim", "dimnames", "class", "levels"))
    str_attr(attributes(object), "*", c("draws", "names", "dim", "dimnames", "class", "nchains", "cache"))
  }

  invisible(NULL)
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export
pillar_shaft.rvar <- function(x, ...) {
  new_pillar_shaft_simple(format(x, color = TRUE, pad_left = " "), align = "right", ...)
}

#' @importFrom pillar format_glimpse
#' @export
format_glimpse.rvar <- function(x, ...) {
  .dim <- dim(x)
  if (length(.dim) > 1) {
    paste0("<rvar[", paste0(.dim, collapse = " x "), "]>")
  } else {
    format(x, ..., trim = TRUE)
  }
}

# type summaries ----------------------------------------------------------

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.rvar <- function(x, ...) {
  "rvar"
}
#' @export
vec_ptype_abbr.rvar_factor <- function(x, ...) {
  "rvar_fct"
}
#' @export
vec_ptype_abbr.rvar_ordered <- function(x, ...) {
  "rvar_ord"
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.rvar <- function(x, ...) {
  rvar_type_full(x, dim1 = FALSE)
}

rvar_type_full <- function(x, dim1 = TRUE) {
  .dim <- dim(draws_of(x))

  dim_str <- if (dim1) {
    paste0("[", paste0(.dim[-1], collapse = ","), "]")
  } else if (length(.dim) > 2) {
    paste0("[,", paste0(.dim[-c(1,2)], collapse = ","), "]")
  } else {
    ""
  }

  chain_str <- if (nchains(x) > 1) {
    paste0(",", nchains(x))
  }

  paste0(rvar_class(x), "<", niterations(x), chain_str, ">", dim_str)
}

rvar_class <- function(x) {
  paste0(
    "rvar",
    if (is_rvar_ordered(x)) "_ordered"
    else if (is_rvar_factor(x)) "_factor"
  )
}


# rvar draws formatting helpers ------------------------------------------------

# formats a draws array for display as individual "variables" (i.e. maintaining
# its dimensions except for the dimension representing draws)
format_rvar_draws <- function(
  draws, ..., pad_left = "", pad_right = "", summary = NULL, digits = 2, color = FALSE, trim = FALSE
) {
  if (length(draws) == 0) {
    return(character())
  }
  summary_functions <- get_summary_functions(draws, summary)
  plus_minus <- summary_functions[[1]] != "modal_category"

  summary_dimensions <- seq_len(length(dim(draws)) - 1) + 1

  # these will be mean/sd, median/mad, mode/entropy, mode/dissent depending on `summary`
  .mean <- .apply_factor(draws, summary_dimensions, summary_functions[[1]])
  .sd <- .apply_factor(draws, summary_dimensions, summary_functions[[2]])

  out <- paste0(
    pad_left,
    format_mean_sd(.mean, .sd, digits = digits, color = color, trim = trim, plus_minus = plus_minus),
    pad_right
  )

  dim(out) <- dim(draws)[summary_dimensions]
  dimnames(out) <- dimnames(draws)[summary_dimensions]
  out
}

format_mean <- function(x, digits = 2, color = FALSE, trim = FALSE) {
  format(x, justify = "right", digits = digits, scientific = 2, trim = trim)
}

format_sd <- function(x, digits = 2, color = FALSE, plus_minus = TRUE) {
  sd_formatted <- format(x, justify = "left", trim = TRUE, digits = digits, scientific = 2)
  sd_string <- if (plus_minus) {
    # \u00b1 = plus/minus sign
    paste0("\u00b1 ", sd_formatted)
  } else {
    paste0("<", sd_formatted, ">")
  }

  if (color) {
    pillar::style_subtle(sd_string)
  } else {
    sd_string
  }
}

format_mean_sd <- function(.mean, .sd, digits = 2, color = FALSE, trim = FALSE, plus_minus = TRUE) {
  format(paste0(
    format_mean(.mean, digits = digits, color = color, trim = trim), " ",
    format_sd(.sd, digits = digits, color = color, plus_minus = plus_minus)),
  justify = if (trim) "none" else "left", trim = trim)
}

format_levels <- function(levels, ordered = FALSE, max_level = NULL, width = getOption("width")) {
  levels <- encodeString(levels)
  n_levels <- length(levels)
  header <- paste(format(n_levels), "levels: ")

  sep <- if (ordered) " < " else " "
  width <- width - (nchar(header, "width") + 3L + 1L + 3L)
  levels_cumwidth <- cumsum(nchar(levels, "width") + nchar(sep, "width"))

  max_level <- max_level %||% if (n_levels <= 1L || levels_cumwidth[n_levels] <= width) {
    n_levels
  } else {
    max(1L, which.max(levels_cumwidth > width) - 1L)
  }
  drop <- n_levels > max_level

  paste0(
    header,
    paste(
      if (drop) c(levels[1L:max(1, max_level - 1)], "...", if (max_level > 1) levels[n_levels])
      else levels,
      collapse = sep
    )
  )
}

# check that summary is a valid name of the type of summary to do and
# return a vector of two elements, where the first is the point summary function
# (mean, median, mode) and the second is the uncertainty function ()
get_summary_functions <- function(draws, summary = NULL) {
  if (is.ordered(draws)) {
    summary = "mode_dissent"
  } else if (is.factor(draws)) {
    summary = "mode_entropy"
  }

  if (is.null(summary)) summary <- getOption("posterior.rvar_summary", "mean_sd")
  switch(summary,
    mean_sd = list(mean = "mean", sd = "sd"),
    median_mad = list(median = "median", mad = "mad"),
    mode_entropy = list(mode = "modal_category", entropy = "entropy"),
    mode_dissent = list(mode = "modal_category", dissent = "dissent"),
    stop_no_call('`summary` must be one of "mean_sd" or "median_mad"')
  )
}

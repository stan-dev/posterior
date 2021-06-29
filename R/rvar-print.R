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
print.rvar <- function(x, ..., summary = NULL, digits = 2, color = TRUE) {
  # \u00b1 = plus/minus sign
  summary_functions <- get_summary_functions(summary)
  summary_string <- paste0(paste(summary_functions, collapse = " \u00b1 "), ":")
  if (color) {
    summary_string <- pillar::style_subtle(summary_string)
  }
  cat0(rvar_type_abbr(x), " ", summary_string, "\n")
  print(format(x, summary = summary, digits = digits, color = FALSE, pad_right = " "), quote = FALSE)
  invisible(x)
}

#' @rdname print.rvar
#' @export
format.rvar <- function(x, ..., summary = NULL, digits = 2, color = FALSE) {
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
    .draws <- .draws[, 1:vec.len]
    ellipsis <- " ..."
  } else {
    ellipsis <- ""
  }

  cat0(" ", rvar_type_abbr(object), "  ",
    paste(format_rvar_draws(.draws, summary = summary), collapse = "  "),
    ellipsis, "\n"
  )

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
    str_attr(attributes(draws_of(object)), "draws_of(*)", c("names", "dim", "dimnames", "class"))
    str_attr(attributes(object), "*", c("draws", "names", "dim", "dimnames", "class", "nchains"))
  }

  invisible(NULL)
}

#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @export
pillar_shaft.rvar <- function(x, ...) {
  new_pillar_shaft_simple(format(x, color = TRUE, pad_left = " "), align = "right", ...)
}

# type summaries ----------------------------------------------------------

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.rvar <- function(x, ...) {
  "rvar"
}

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.rvar <- function(x, ...) {
  rvar_type_abbr(x, dim1 = FALSE)
}

rvar_type_abbr <- function(x, dim1 = TRUE) {
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

  paste0("rvar<", niterations(x), chain_str, ">", dim_str)
}


# rvar draws formatting helpers ------------------------------------------------

# formats a draws array for display as individual "variables" (i.e. maintaining
# its dimensions except for the dimension representing draws)
format_rvar_draws <- function(draws, ..., pad_left = "", pad_right = "", summary = NULL, digits = 2, color = FALSE) {
  if (prod(dim(draws)) == 0) {
    # NULL: no draws
    return(NULL)
  }
  summary_functions <- get_summary_functions(summary)

  summary_dimensions <- seq_len(length(dim(draws)) - 1) + 1

  # these will be mean/sd or median/mad depending on `summary`
  .mean <- apply(draws, summary_dimensions, summary_functions[[1]])
  .sd <- apply(draws, summary_dimensions, summary_functions[[2]])
  out <- paste0(pad_left, format_mean_sd(.mean, .sd, digits = digits, color = color), pad_right)

  dim(out) <- dim(draws)[summary_dimensions]
  dimnames(out) <- dimnames(draws)[summary_dimensions]
  out
}

format_mean <- function(x, digits = 2, color = FALSE) {
  format(x, justify = "right", digits = digits, scientific = 2)
}

format_sd <- function(x, digits = 2, color = FALSE) {
  # \u00b1 = plus/minus sign
  sd_string <- paste0("\u00b1 ", format(x, justify = "left", trim = TRUE, digits = digits, scientific = 2))
  if (color) {
    pillar::style_subtle(sd_string)
  } else {
    sd_string
  }
}

format_mean_sd <- function(.mean, .sd, digits = 2, color = FALSE) {
  format(paste0(
    format_mean(.mean, digits = digits, color = color), " ",
    format_sd(.sd, digits = digits, color = color)),
  justify = "left")
}

# check that summary is a valid name of the type of summary to do and
# return a vector of two elements, where the first is mean or median and the
# second is sd or mad
get_summary_functions <- function(summary = NULL) {
  if (is.null(summary)) summary <- getOption("posterior.rvar_summary", "mean_sd")
  switch(summary,
    mean_sd = c("mean", "sd"),
    median_mad = c("median", "mad"),
    stop_no_call('`summary` must be one of "mean_sd" or "median_mad"')
  )
}

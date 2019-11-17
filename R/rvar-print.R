#' Print or format a random variable
#'
#' Printing and formatting methods for [rvar]s.
#'
#' @param x,object An [rvar].
#' @param color Whether or not to use color when formatting the output. If `TRUE`,
#' the [pillar::style_num()] functions may be used to produce strings containing
#' control sequences to produced colored output on the terminal.
#' @param vec.len Numeric (>= 0) indicating how many ‘first few’ elements are
#' displayed of each vector. If `NULL`, defaults to `getOption("str")$vec.len`,
#' which defaults to 4.
#' @param ... Further arguments passed to other functions.
#'
#' @details The `"rvar"` class represents random variables of arbitrary objects.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @export
print.rvar <- function(x, ...) {
  # \u00b1 = plus/minus sign
  cat0(rvar_type_abbr(x), " ", pillar::style_subtle("mean\u00b1sd:"), "\n")
  print(format(x, color = FALSE), quote = FALSE)
  invisible(x)
}

#' @importFrom methods setMethod
setMethod("show", "rvar", function(object) print.rvar(object))

#' @rdname print.rvar
#' @export
format.rvar <- function(x, ..., color = FALSE) {
  format_rvar_draws(draws_of(x), ..., color = color)
}

#' @rdname print.rvar
#' @export
str.rvar <- function(object, ..., vec.len = NULL) {
  .draws <- draws_of(object)
  vec.len <- vec.len %||% getOption("str")$vec.len %||% 4

  # flatten all the non-draws dimensions
  .dim <- dim(.draws)
  dim(.draws) <- c(prod(.dim[-length(.dim)]), .dim[length(.dim)])

  if (dim(.draws)[[1]] > vec.len) {
    .draws <- .draws[1:vec.len, ]
    ellipsis <- " ..."
  } else {
    ellipsis <- ""
  }

  cat0(" ", rvar_type_abbr(object), " " , paste(format_rvar_draws(.draws), collapse = " "), ellipsis, "\n")
  invisible(object)
}


# pillar methods (for printing in tibbles) --------------------------------

#' @importFrom pillar pillar_shaft
#' @rdname print.rvar
#' @export
pillar_shaft.rvar <- function(x, ...) {
  out <- format(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.rvar <- function(x) TRUE


# type summaries ----------------------------------------------------------

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.rvar <- function(x, ...) "rvar"

#' @importFrom vctrs vec_ptype_full
#' @export
vec_ptype_full.rvar <- function(x, ...) rvar_type_abbr(x, dim1 = FALSE)

rvar_type_abbr <- function(x, dim1 = TRUE) {
  .dim <- dim(draws_of(x))

  dim_str <- if (dim1) {
    paste0("[", paste0(.dim[-length(.dim)], collapse = ","), "]")
  } else if (length(.dim) > 2) {
    paste0("[,", paste0(.dim[-c(1,length(.dim))], collapse = ","), "]")
  } else {
    ""
  }

  paste0("rvar<", ndraws(x), ">", dim_str)
}


# rvar draws formatting helpers ------------------------------------------------

# formats a draws array for display as individual "variables" (i.e. maintaining
# its dimensions except for the dimension representing draws)
format_rvar_draws <- function(draws, ...) UseMethod("format_rvar_draws")

format_rvar_draws.default <- function(draws, ...) {
  stop("IMPLEMENT ME")
}

format_rvar_draws.numeric <- function(draws, ..., color = FALSE) {
  if (dim(draws)[length(dim(draws))] == 0) {
    # NULL: no draws
    return(NULL)
  }

  summary_dimensions <- seq_len(length(dim(draws)) - 1)

  .mean <- apply(draws, summary_dimensions, mean)
  .sd <- apply(draws, summary_dimensions, sd)
  out <- format_mean_sd(.mean, .sd, color = color)

  dim(out) <- dim(draws)[summary_dimensions]
  dimnames(out) <- dimnames(draws)[summary_dimensions]
  out
}

format_rvar_draws.logical <- format_rvar_draws.numeric

format_mean <- function(x, color = FALSE) {
  format(x, justify = "right", digits = 2, scientific = 2)
}

format_sd <- function(x, color = FALSE) {
  # \u00b1 = plus/minus sign
  sd_string <- paste0("\u00b1", format(x, justify = "left", trim = TRUE, digits = 2, scientific = 2))
  if (color) {
    pillar::style_subtle(sd_string)
  } else {
    sd_string
  }
}

format_mean_sd <- function(.mean, .sd, color = FALSE) {
  format(paste0(format_mean(.mean, color = color), format_sd(.sd, color = color)), justify = "left")
}


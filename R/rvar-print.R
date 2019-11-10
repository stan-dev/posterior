#' Print or format a random variable
#'
#' Printing and formatting methods for random variables of arbitrary objects
#'
#' @param x A list or vector where each entry represents a draw from a distribution
#' @param color Whether or not to use color when formatting the output
#'
#' @details The `"rvar"` class represents random variables of arbitrary objects.
#'
#' @return An object of class `"rvar"` representing a random variable.
#'
#' @export
print.rvar = function(x, ...) {
  cat0(rvar_type_abbr(x), ":\n")
  print(format(x, color = FALSE), quote = FALSE)
  invisible(x)
}

#' @rdname print.rvar
#' @export
format.rvar = function(x, ..., color = FALSE) {
  format_draws(field(x, 1), ..., color = color)
}

#' @rdname print.rvar
#' @export
str.rvar <- function(x, ..., vec.len = NULL, width = NULL) {
  .draws = field(x, 1)
  width = width %||% getOption("width")
  vec.len = vec.len %||% getOption("str")$vec.len

  # flatten all the non-draws dimensions
  .dim = dim(.draws)
  dim(.draws) = c(prod(.dim[-length(.dim)]), .dim[length(.dim)])

  if (dim(.draws)[[1]] > vec.len) {
    .draws = .draws[1:vec.len, ]
    ellipsis = " ..."
  } else {
    ellipsis = ""
  }

  cat0(" ", rvar_type_abbr(x), " " , paste(format_draws(.draws), collapse = " "), ellipsis, "\n")
  invisible(x)
}


# pillar methods ----------------------------------------------------------

#' @importFrom pillar pillar_shaft
#' @rdname print.rvar
#' @export
pillar_shaft.rvar <- function(x, ...) {
  out = format(x, color = TRUE)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.rvar <- function(x) TRUE



# draws formatting methods ------------------------------------------------

# formats a draws array for display as individual "variables" (i.e. maintaining
# its dimensions except for the dimension representing draws)
format_draws = function(draws, ...) UseMethod("format_draws")

format_draws.default = function(draws, ...) {
  stop("IMPLEMENT ME")
}

format_draws.numeric = function(draws, ..., color = FALSE) {
  if (dim(draws)[length(dim(draws))] == 0) {
    # NULL: no draws
    return(NULL)
  }

  summary_dimensions = seq_len(length(dim(draws)) - 1)

  .mean = apply(draws, summary_dimensions, mean)
  .sd = apply(draws, summary_dimensions, sd)
  out = format_mean_sd(.mean, .sd, color = color)

  dim(out) = dim(draws)[summary_dimensions]
  dimnames(out) = dimnames(draws)[summary_dimensions]
  out
}

format_draws.logical = format_draws.numeric

# formatting helpers ------------------------------------------------------

format_mean = function(x, color = FALSE) {
  format(x, justify = "right", digits = 2, scientific = 2)
}

format_sd = function(x, color = FALSE) {
  sd_string = paste0("±", format(x, justify = "right", digits = 2, scientific = 2))
  if (color) {
    pillar::style_subtle(sd_string)
  } else {
    sd_string
  }
}

format_mean_sd = function(.mean, .sd, color = FALSE) {
  paste0(format_mean(.mean, color = color), format_sd(.sd, color = color))
}

# succinct type summary for an rvar
rvar_type_abbr = function(x) {
  .draws = field(x, 1)
  .dim = dim(.draws)

  dim_str = paste0("1:", .dim[-length(.dim)], collapse = ", ")
  paste0("rvar<", ndraws(x), "> [", dim_str, "]")
}

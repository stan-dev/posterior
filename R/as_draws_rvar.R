#' The `draws_rvar` format
#'
#' @name draws_rvar
#' @family formats
#'
#' @templateVar draws_format draws_rvar
#' @templateVar base_class "list"
#' @template draws_format-skeleton
#' @template args-format-nchains
#'
#' @details Objects of class `"draws_rvar"` are lists of [`rvar`] objects.
#' See **Examples**.
#'
NULL

#' @rdname draws_rvar
#' @export
as_draws_rvar <- function(x, ...) {
  UseMethod("as_draws_rvar")
}

#' @rdname draws_array
#' @export
as_draws_rvar.array <- function(x, ...) {
  x <- as_draws_df(x)
  if (ndraws(x) == 0L) {
    return(empty_draws_array(variables(x)))
  }
  vars <- unique(gsub(pattern = "\\[.+\\]$", replacement = "", variables(x)))
  n_chain <- nchains(x)
  x <- lapply(vars, function(var) {
    rvar_data <- subset_draws(x, variable = var, regex = TRUE)
    class(rvar_data) <- "data.frame"
    rvar_data[c(".chain", ".iteration", ".draw")] <- NULL
    if (ncol(rvar_data) == 1L) {
      rvar_dim <- 1L
      rvar_dimname <- NULL
    } else {
      var_names <- colnames(rvar_data)
      var_curr <- gsub("\\[.+\\]$", "", var_names[1])
      ind_old <- do.call(
        Map, c(c, strsplit(gsub("^.+\\[(.+)\\]$", "\\1", var_names), ","))
      )
      ind_range <- lapply(ind_old, function(ind) {
        int <- SW(as.integer(ind))
        if (rlang::is_integerish(int, finite = TRUE)) {
          seq_len(max(int))
        } else {
          unique(ind)
        }
      })
      ind_grid <- expand.grid(ind_range)
      var_grid <- paste0(var_curr, "[", do.call(paste, c(ind_grid, sep=",")), "]")
      var_na <- paste0(var_curr, "_NA")
      var_grid[!(var_grid %in% var_names)] <- var_na
      rvar_data[[var_na]] <- NA
      rvar_data <- rvar_data[var_grid]
      rvar_dim <- unname(lengths(ind_range))
      rvar_dimname <- unname(lapply(ind_range, function(ind) {
        if (is.character(ind)) ind
        else NULL
      }))
    }
    rvar(as.matrix(rvar_data), dim = rvar_dim, nchains = n_chain, dimnames = rvar_dimname)
  })
  names(x) <- vars
  class(x) <- class_draws_rvar()
  x
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.default <- function(x, ...) {
  x <- as_draws_df(x)
  x <- as_draws_array.data.frame(x)
  as_draws_rvar.array(x, ...)
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.draws_rvar <- function(x, ...) {
  x
}

#' @rdname draws_rvar
#' @export
as_draws_rvar.rvar <- function(x, ...) {
  x <- as.list(x)
  if (is.null(names(x))) {
    names(x) <- default_variables(length(x))
  }
  class(x) <- class_draws_rvar()
  x
}

#' @rdname draws_rvar
#' @export
draws_rvar <- function(..., .nchains = 1) {
  x <- lapply(list(...), function(x) {
    if (is_rvar(x)) x
    else rvar(x, nchains = .nchains)
  })
  if (!rlang::is_named(x)) {
    stop_no_call("All variables must be named.")
  }
  x <- conform_rvar_ndraws_nchains(x)
  class(x) <- class_draws_rvar()
  x
}

class_draws_rvar <- function() {
  c("draws_rvar", "draws", "list")
}

#' @rdname draws_rvar
#' @export
is_draws_rvar <- function(x) {
  inherits(x, "draws_rvar")
}

#' @export
`[.draws_rvar` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod("[")
  class(out) <- class(x)
  out
}

# create an empty draws_rvar object
empty_draws_rvar <- function(variables = NULL, nchains = 0) {
  assert_character(variables, null.ok = TRUE)
  assert_number(nchains, lower = 0)
  out <- named_list(variables %||% character(0), rvar())
  class(out) <- class_draws_rvar()
  out
}

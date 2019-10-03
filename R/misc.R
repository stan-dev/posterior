# create a named list using object names
nlist <- function(...) {
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}

seq_rows <- function(x) {
  seq_len(NROW(x))
}

seq_cols <- function(x) {
  seq_len(NCOL(x))
}

seq_dim <- function(x, dim) {
  dim <- as_one_numeric(dim)
  if (dim == 1) {
    len <- NROW(x)
  } else if (dim == 2) {
    len <- NCOL(x)
  } else {
    len <- dim(x)[dim]
  }
  if (length(len) == 1L && !isNA(len)) {
    out <- seq_len(len)
  } else {
    out <- integer(0)
  }
  out
}

'%||%' <- function(x, y) {
  if (is.null(x)) x <- y
  x
}

# cat with without separating elements
cat0 <- function(..., file = "", fill = FALSE, labels = NULL, append = FALSE) {
  cat(..., sep = "", file = file, fill = fill, labels = labels, append = append)
}

# coerce 'x' to a single logical value
as_one_logical <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.logical(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse2(s)
    stop2("Cannot coerce '", s, "' to a single logical value.")
  }
  x
}

# coerce 'x' to a single integer value
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse2(s)
    stop2("Cannot coerce '", s, "' to a single integer value.")
  }
  x
}

# coerce 'x' to a single numeric value
as_one_numeric <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.numeric(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse2(s)
    stop2("Cannot coerce '", s, "' to a single numeric value.")
  }
  x
}

# coerce 'x' to a single character string
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.character(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse2(s)
    stop2("Cannot coerce '", s, "' to a single character value.")
  }
  x
}

# check if two objects are equal
is_equal <- function(x, y, ...) {
  isTRUE(all.equal(x, y, ...))
}

# prettily deparse an expression
# @return a single character string
deparse2 <- function(x, max_chars = NULL, max_wsp = 1L) {
  out <- collapse(deparse(x))
  out <- rm_wsp(out, max_wsp)
  assert_int(max_chars, null.ok = TRUE)
  if (isTRUE(max_chars > 0L)) {
    out <- substr(out, 1L, max_chars)
  }
  out
}

#' @export
quantile.vctrs_rray <- function(x, ...) {
  # TODO: make a PR to rray for this?
  quantile(as.vector(x), ...)
}

#' @export
median.vctrs_rray <- function(x, ...) {
  # TODO: make a PR to rray for this?
  median(as.vector(x), ...)
}

isNA <- function(x) {
  length(x) == 1L && is.na(x)
}

stop2 <- function(...) {
  stop(..., call. = FALSE)
}

warning2 <- function(...) {
  warning(..., call. = FALSE)
}

SW <- function(expr) {
  base::suppressWarnings(expr)
}

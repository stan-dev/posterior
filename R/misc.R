# initialize a named list
# @param names names of the elements
# @param values optional values of the elements
named_list <- function(names, values = NULL) {
  if (!is.null(values)) {
    if (length(values) <= 1L) {
      values <- replicate(length(names), values)
    }
    values <- as.list(values)
    stopifnot(length(values) == length(names))
  } else {
    values <- vector("list", length(names))
  }
  setNames(values, names)
}

# unlist lapply output
ulapply <- function(X, FUN, ..., recursive = TRUE, use.names = TRUE) {
  unlist(lapply(X, FUN, ...), recursive, use.names)
}

seq_rows <- function(x) {
  seq_len(NROW(x))
}

seq_cols <- function(x) {
  seq_len(NCOL(x))
}

# selectively drop one-level dimensions of an array and/or reset object classes
drop_dims_or_classes <- function(x, dims = NULL, reset_class = FALSE) {
  assert_array(x)
  assert_integerish(dims, null.ok = TRUE)
  reset_class <- as_one_logical(reset_class)
  old_dims <- dim(x)
  # proceed to drop dimensions if the input array has any non-NULL dimensions
  if (length(old_dims)) {
    # base::drop if all one-level dimensions are to be dropped non-selectively
    if (is.null(dims) || setequal(dims, which(old_dims == 1L))) {
      x <- drop(x)
      # custom drop if certain one-level dimensions are to be dropped selectively
    } else {
      dim(x) <- old_dims[-dims]
      old_dimnames <- dimnames(x)
      # if all names of new dimnames are empty strings (""), set them to NULL
      new_dimnames <- old_dimnames[-dims]
      if (all(names(new_dimnames) == "")) {
        names(new_dimnames) <- NULL
      }
      dimnames(x) <- new_dimnames
    }
  }
  # optionally, set class to NULL and let R decide appropriate classes
  if (reset_class) {
    class(x) <- NULL
  }
  x
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
    s <- deparse_pretty(s)
    stop_no_call("Cannot coerce '", s, "' to a single logical value.")
  }
  x
}

# coerce 'x' to a single integer value
as_one_integer <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.integer(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse_pretty(s)
    stop_no_call("Cannot coerce '", s, "' to a single integer value.")
  }
  x
}

# coerce 'x' to a single numeric value
as_one_numeric <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- SW(as.numeric(x))
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse_pretty(s)
    stop_no_call("Cannot coerce '", s, "' to a single numeric value.")
  }
  x
}

# coerce 'x' to a single character string
as_one_character <- function(x, allow_na = FALSE) {
  s <- substitute(x)
  x <- as.character(x)
  if (length(x) != 1L || anyNA(x) && !allow_na) {
    s <- deparse_pretty(s)
    stop_no_call("Cannot coerce '", s, "' to a single character value.")
  }
  x
}

# check if all inputs are NULL
all_null <- function(...) {
  all(ulapply(list(...), is.null))
}

# check if two objects are equal
is_equal <- function(x, y, ...) {
  isTRUE(all.equal(x, y, ...))
}

# move elements to the start of a named object
move_to_start <- function(x, start) {
  assert_named(x)
  start <- intersect(names(x), start)
  if (!length(start)) {
    return(x)
  }
  out <- x[c(start, setdiff(names(x), start))]
  class(out) <- class(x)
  out
}

# prettily deparse an expression
# @return a single character string
deparse_pretty <- function(x, max_chars = NULL, max_wsp = 1L) {
  out <- collapse(deparse(x))
  out <- rm_wsp(out, max_wsp)
  assert_int(max_chars, null.ok = TRUE)
  if (isTRUE(max_chars > 0L)) {
    out <- substr(out, 1L, max_chars)
  }
  out
}

# remove NULL elements from a list
remove_null <- function(x) {
  Filter(Negate(is.null), x)
}

# remove whitespaces from strings
rm_wsp <- function(x, max_wsp = 0) {
  assert_character(x)
  assert_int(max_wsp)
  wsp <- collapse(rep(" ", max_wsp))
  gsub("[ \t\r\n]+", wsp, x, perl = TRUE)
}

# collapse a character vector
collapse <- function(..., sep = "") {
  paste(..., sep = sep, collapse = "")
}

# like 'eval' but parses characters before evaluation
eval2 <- function(expr, envir = parent.frame(), ...) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  }
  eval(expr, envir, ...)
}

# wrapper around replicate but without simplifying
repl <- function(expr, n) {
  replicate(n, expr, simplify = FALSE)
}

isNA <- function(x) {
  length(x) == 1L && is.na(x)
}

is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

# collapse variables via commas for pretty printing
comma <- function(...) {
  paste0("{", paste0("'", c(...), "'", collapse = ", "), "}")
}

stop_no_call <- function(...) {
  stop(..., call. = FALSE)
}

warning_no_call <- function(...) {
  warning(..., call. = FALSE)
}

SW <- function(expr) {
  base::suppressWarnings(expr)
}

# escape all special characters in character strings
escape_all <- function(x) {
  specials <- "(\\.|\\*|\\+|\\?|\\^|\\$|\\(|\\)|\\[|\\]|\\|)"
  gsub(specials, "\\\\\\1", x)
}

# numerically stable version of log(sum(exp(x)))
log_sum_exp <- function(x) {
  max <- max(x)
  sum <- sum(exp(x - max))
  max + log(sum)
}

# simple version of destructuring assignment
`%<-%` <- function(vars, values, envir = parent.frame()) {
  vars <- as.character(substitute(vars)[-1])
  for (i in seq_along(vars)) {
    assign(vars[[i]], values[[i]], envir = envir)
  }
  invisible(NULL)
}

# get names of all the functions of a package
package_function_names <- function(package) {
  unclass(utils::lsf.str(envir = asNamespace(package), all = TRUE))
}

# return true if there is an S3 method for the
# given function and class signature
has_s3_method <- function(f, signature) {
  for (class in signature) {
    if (!is.null(utils::getS3method(f, class, optional = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}


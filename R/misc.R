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

# selectively drop dimensions of arrays
drop_dims <- function(x, dims = NULL) {
  assert_array(x)
  assert_integerish(dims, null.ok = TRUE)
  old_dims <- dim(x)
  if (is.null(dims)) {
    dims <- old_dims[old_dims == 1L]
  } else {
    assert_true(all(old_dims[dims] <= 1L))
  }
  if (!length(dims)) {
    return(x)
  }
  old_dimnames <- dimnames(x)
  dim(x) <- old_dims[-dims]
  dimnames(x) <- old_dimnames[-dims]
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
deparse2 <- function(x, max_chars = NULL, max_wsp = 1L) {
  out <- collapse(deparse(x))
  out <- rm_wsp(out, max_wsp)
  assert_int(max_chars, null.ok = TRUE)
  if (isTRUE(max_chars > 0L)) {
    out <- substr(out, 1L, max_chars)
  }
  out
}

# like 'eval' but parses characters before evaluation
eval2 <- function(expr, envir = parent.frame(), ...) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  }
  eval(expr, envir, ...)
}

# Execute a Function Call
#
# Execute a function call similar to \code{\link{do.call}}, but without
# deparsing function arguments.
#
# @param what Either a function or a non-empty character string naming the
#   function to be called.
# @param args A list of arguments to the function call. The names attribute of
#   \code{args} gives the argument names.
# @param pkg Optional name of the package in which to search for the
#   function if \code{what} is a character string.
#
# @return The result of the (evaluated) function call.
#
# @keywords internal
# @export
do_call <- function(what, args, pkg = NULL) {
  call <- ""
  if (length(args)) {
    if (!is.list(args)) {
      stop2("'args' must be a list.")
    }
    fun_args <- names(args)
    if (is.null(fun_args)) {
      fun_args <- rep("", length(args))
    } else {
      nzc <- nzchar(fun_args)
      fun_args[nzc] <- paste0("`", fun_args[nzc], "` = ")
    }
    names(args) <- paste0(".x", seq_along(args))
    call <- paste0(fun_args, names(args), collapse = ",")
  } else {
    args <- list()
  }
  if (is.function(what)) {
    args$.fun <- what
    what <- ".fun"
  } else {
    what <- paste0("`", as_one_character(what), "`")
    if (!is.null(pkg)) {
      what <- paste0(as_one_character(pkg), "::", what)
    }
  }
  call <- paste0(what, "(", call, ")")
  eval2(call, envir = args, enclos = parent.frame())
}

# create continuous indices from 1 to length(unique(x))
index_continuously <- function(x) {
  # TODO: use the order of appearence?
  as.integer(factor(x))
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

stop2 <- function(...) {
  stop(..., call. = FALSE)
}

warning2 <- function(...) {
  warning(..., call. = FALSE)
}

SW <- function(expr) {
  base::suppressWarnings(expr)
}

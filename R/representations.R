#' Attempt to transform to a posterior object
#'
#' @param x An \R object to be converted.
#' @param class optional posterior class to transform to.
#'   If \code{NULL} (the default), the class most similar to the
#'   structure of \code{x} is chosen.
#'
#' @import rray
#' @export
as_posterior <- function(x, class = NULL) {
  assert_choice(class, all_posterior_classes(), null.ok = TRUE)
  if (!is_posterior(x)) {
    x <- .as_posterior(x)
  }
  if (!is.null(class)) {
    x <- posterior_transform(x, class = class)
  }
  x
}

# convert a non-posterior object to a posterior object
.as_posterior <- function(x) {
  assert_false(is_posterior(x))
  if (is_matrix_like(x)) {
    message("Converting to a posterior matrix.")
    x <- as_rray(x)
    new_dimnames <- list(draw = NULL, par = NULL)
    if (!is.null(dimnames(x)[[2]])) {
      new_dimnames[[2]] <- dimnames(x)[[2]]
    } else {
      # TODO: how to call parameters by default?
      new_dimnames[[2]] <- paste0("X", seq_cols(x))
    }
    # TODO: use existing row names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    dimnames(x) <- new_dimnames
  } else if (is_3d_array_like(x)) {
    message("Converting to a posterior array.")
    x <- as_rray(x)
    new_dimnames <- list(draw = NULL, chain = NULL, par = NULL)
    if (!is.null(dimnames(x)[[3]])) {
      new_dimnames[[3]] <- dimnames(x)[[3]]
    } else {
      # TODO: how to call parameters by default?
      new_dimnames[[3]] <- paste0("X", seq_dim(x, 3))
    }
    # TODO: use existing row/col names in any way?
    new_dimnames[[1]] <- as.character(seq_rows(x))
    new_dimnames[[2]] <- as.character(seq_cols(x))
    dimnames(x) <- new_dimnames
  } else {
    stop2("Don't know how to transform an object of class ",
          "'", class(x)[1L], "' to a posterior object.")
  }
  x
}

#' @export
as_posterior_matrix <- function(x) {
  as_posterior(x, "matrix")
}

#' @export
as_posterior_array <- function(x) {
  as_posterior(x, "array")
}

# tranform posterior object to another representation
posterior_transform <- function(x, class) {
  old_class <- posterior_class(x)
  assert_choice(class, all_posterior_classes())
  fun <- paste0("posterior_", old_class, "_to_", class)
  fun <- get(fun, pos = asNamespace("posterior"))
  fun(x)
}

# all possible posterior represenations
all_posterior_classes <- function() {
  c("matrix", "array")
}

# extract the posterior class
posterior_class <- function(x) {
  if (is_posterior_matrix(x)) {
    out <- "matrix"
  } else if (is_posterior_array(x)) {
    out <- "array"
  } else {
    stop2("Posterior type not recognized.")
  }
  out
}

#' @export
is_posterior <- function(x) {
  is_posterior_matrix(x) || is_posterior_array(x)
}

#' @export
is_posterior_matrix <- function(x) {
  is_rray(x) && is_equal(names(dimnames(x)), c("draw", "par"))
}

#' @export
is_posterior_array <- function(x) {
  is_rray(x) && is_equal(names(dimnames(x)), c("draw", "chain", "par"))
}

is_matrix_like <- function(x) {
  is.matrix(x) || is_rray(x) && length(dim(x)) == 2L
}

is_3d_array_like <- function(x) {
  (is.array(x) || is_rray(x)) && length(dim(x)) == 3L
}

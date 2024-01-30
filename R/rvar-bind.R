# concatenation and binding ----------------------------------------------------

#' @export
c.rvar <- function(...) {
  common_type <- vctrs::vec_ptype_common(...)
  if (!is_rvar(common_type)) {
    # if the common type of the arguments is not an rvar, fall back to the
    # vctrs implementation of c()
    return(vctrs::vec_c(...))
  }

  args <- list(...)
  # drop NULLs (for compatibility with R < 4: in R >= 4, NULLs will have already been dropped)
  args[vapply(args, is.null, logical(1))] <- NULL

  out <- make_1d(args[[1]], names(args)[[1]])

  # process remaining args in succession, binding them to the output
  for (i in seq_along(args)[-1]) {
    arg_name <- names(args)[[i]]
    arg <- make_1d(vec_cast(args[[i]], common_type), arg_name)
    out <- broadcast_and_bind_rvars(out, arg)
  }

  out
}

#' @export
rbind.rvar <- function(..., deparse.level = 1) {
  # deparse.level is not correctly passed here by the default rbind
  # implementation in R < 4.4, so we grab it from the calling environment
  deparse.level <- rlang::caller_env()$deparse.level %||% deparse.level
  bind_rvars(list(...), as.list(substitute(list(...))[-1]), deparse.level)
}

#' @export
cbind.rvar <- function(..., deparse.level = 1) {
  # deparse.level is not correctly passed here by the default cbind
  # implementation in R < 4.4, so we grab it from the calling environment
  deparse.level <- rlang::caller_env()$deparse.level %||% deparse.level
  bind_rvars(list(...), as.list(substitute(list(...))[-1]), deparse.level, axis = 2)
}

#' bind a list of objects together, as in cbind or rbind (depending on `axis`),
#' converting to rvars as needed
#' @noRd
bind_rvars <- function(args, arg_exprs, deparse.level = 1, axis = 1) {
  if (any(sapply(args, is.data.frame))) {
    # if there is a data frame in args, we should just make the first arg
    # into a data frame and then use the data frame bind implementation

    # data frames always deparse at level 2 (expressions get named)
    args <- deparse_names(args, arg_exprs, deparse.level = 2)
    args[[1]] <- as.data.frame(make_at_least_2d(args[[1]], axis, names(args)[[1]]))
    bind <- if (axis == 1) rbind else cbind
    return(do.call(bind, args))
  }

  args <- deparse_names(args, arg_exprs, deparse.level)

  out <- make_at_least_2d(as_rvar(args[[1]]), axis, names(args)[[1]])

  # process remaining args in succession, binding them to the output
  for (i in seq_along(args)[-1]) {
    arg_name <- names(args)[[i]]
    arg <- make_at_least_2d(as_rvar(args[[i]]), axis, arg_name)
    out <- broadcast_and_bind_rvars(out, arg, axis)
  }

  out
}


# helpers: concatenation and binding --------------------------------------

#' broadcast two rvars to compatible dimensions and bind along the `axis` dimension
#' @noRd
broadcast_and_bind_rvars <- function(x, y, axis = 1) {
  UseMethod("broadcast_and_bind_rvars")
}

#' @export
broadcast_and_bind_rvars.rvar <- function(x, y, axis = 1) {
  # TODO: should really replace this with something that takes a list of rvars,
  # conforms their chains, broadcasts them all to common dimensions and uses a
  # single abind to bind them.
  if (!length(x)) return(y)
  if (!length(y)) return(x)

  draws_axis <- axis + 1 # because first dim is draws

  # conform nchains
  # (don't need to do draws here since that's part of the broadcast below)
  c(x, y) %<-% conform_rvar_nchains(list(x, y))

  # broadcast each array to the desired dimensions
  # (except along the axis we are binding along)
  draws_x <- draws_of(x)
  draws_y <- draws_of(y)
  new_dim <- dim2_common(dim(draws_x), dim(draws_y))

  new_dim[draws_axis] <- dim(draws_x)[draws_axis]
  draws_x <- broadcast_array(draws_x, new_dim)

  new_dim[draws_axis] <- dim(draws_y)[draws_axis]
  draws_y <- broadcast_array(draws_y, new_dim)

  # factors may not bind properly with abind, so convert them to characters first
  if (is.factor(draws_x)) draws_x <- copy_dims(draws_x, as.character(draws_x))
  if (is.factor(draws_y)) draws_y <- copy_dims(draws_y, as.character(draws_y))

  # bind along desired axis
  result <- new_rvar(
    abind(draws_x, draws_y, along = draws_axis, use.dnns = TRUE),
    .nchains = nchains(x)
  )
}

#' @export
broadcast_and_bind_rvars.rvar_factor <- function(x, y, axis = 1) {
  result <- NextMethod()
  combine_rvar_factor_levels(
    result, list(levels(x), levels(y)), is_rvar_ordered(x) && is_rvar_ordered(y)
  )
}


#' Deparse argument names roughly following the rules of the deparse.level
#' argument to cbind / rbind
#' @importFrom rlang as_name as_label
#' @noRd
deparse_names <- function(args, arg_exprs, deparse.level) {
  # give arguments names if needed
  if (deparse.level > 0) {
    if (is.null(names(args))) {
      names(args) <- rep("", length(args))
    }
    for (i in seq_along(arg_exprs)) {
      arg_name <- names(args)[[i]]
      arg_expr <- arg_exprs[[i]]
      if (!isTRUE(nzchar(arg_name))) {
        if (deparse.level == 1 && is.name(arg_expr)) {
          names(args)[[i]] <- as_name(arg_expr)
        } else if (deparse.level > 1) {
          names(args)[[i]] <- as_label(arg_expr)
        }
      }
    }
    if (all(names(args) == "")) {
      names(args) = NULL
    }
  }
  args
}

#' restructure a variable for concatenation with `c()` by ensuring that `x` has
#' only 1 dimension and merging the high-level `name` with names in `x`
#' @noRd
make_1d <- function(x, name) {
  if (length(dim(x)) > 1) {
    dim(x) <- length(x)
  }
  if (isTRUE(nzchar(name))) {
    # name provided, merge with names in the vector
    if (length(x) > 1) {
      if (!length(names(x))) names(x) <- rep("", length(x))
      empty_names <- !nzchar(names(x))
      names(x)[empty_names] <- seq_along(x)[empty_names]
      names(x)[!empty_names] <- paste0(".", names(x)[!empty_names])
      names(x) <- paste0(name, names(x))
    } else {
      names(x) <- name
    }
  }
  x
}

#' restructure a variable for binding along `axis` with rbind (axis = 1) or
#' cbind (axis = 2). Ensures that `x` has at least two dimensions (filling
#' setting extra dimension `axis` to 1 if needed) and if `x` is vector-like
#' (is 1-dimensional), sets the name of the newly-added dimension to `axis_name`
#' @noRd
make_at_least_2d <- function(x, axis, axis_name) {
  if (length(dim(x)) <= 1) {
    # input is a vector, turn it into a matrix
    if (axis == 1) { # rbind
      dim(x) = c(1, length(x))
    } else {         # cbind
      dim(x) = c(length(x), 1)
    }
    if (isTRUE(nchar(axis_name) > 0)) {
      dimnames(x)[[axis]] <- axis_name
    }
  }
  x
}

#' Bind `draws` objects together
#'
#' Bind multiple [`draws`] objects together to form a single `draws` object.
#'
#' @param x (draws) A [`draws`] object. The draws format of `x` will define the
#'   format of the returned draws object.
#' @param ... (draws) Additional [`draws`] objects to bind to `x`.
#' @param along (string) The dimension along which draws objects should be bound
#'   together. Possible values are `"variable"` (the default), `"chain"`,
#'   `"iteration"`, and `"draw"`. Not all options are supported for all input
#'   formats.
#' @template return-draws
#'
#' @examples
#' x1 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
#' x2 <- draws_matrix(alpha = rnorm(5), beta = rnorm(5))
#' ndraws(x1)
#' ndraws(x2)
#' x3 <- bind_draws(x1, x2, along = "draw")
#' ndraws(x3)
#'
#' x4 <- draws_matrix(theta = rexp(5))
#' x5 <- bind_draws(x1, x4, along = "variable")
#' variables(x5)
#'
#' @importFrom abind abind
#' @export
bind_draws <- function(x, ...) {
  UseMethod("bind_draws")
}

#' @rdname bind_draws
#' @export
bind_draws.draws_matrix <- function(x, ..., along = "variable") {
  along <- validate_along(along)
  dots <- list(...)
  if (!length(dots)) {
    return(as_draws_matrix(x))
  }
  dots <- c(list(x), dots)
  dots <- remove_null(dots)
  dots <- lapply(dots, as_draws_matrix)
  dots <- lapply(dots, repair_draws)
  if (along == "variable") {
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(abind, c(dots, along = 2L))
    attr(out, "nchains") <- nchains(dots[[1]])
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(abind, c(dots, along = 1L))
    attr(out, "nchains") <- sum(sapply(dots, nchains))
  } else if (along == "iteration") {
    stop_no_call("Cannot bind 'draws_matrix' objects along 'iteration'.")
  } else if (along %in% c("draw")) {
    check_same_fun_output(dots, variables)
    out <- do.call(abind, c(dots, along = 1L))
    attr(out, "nchains") <- 1L
  }
  as_draws_matrix(out)
}

#' @rdname bind_draws
#' @export
bind_draws.draws_array <- function(x, ..., along = "variable") {
  along <- validate_along(along)
  dots <- list(...)
  if (!length(dots)) {
    return(as_draws_array(x))
  }
  dots <- c(list(x), dots)
  dots <- remove_null(dots)
  dots <- lapply(dots, as_draws_array)
  dots <- lapply(dots, repair_draws)
  if (along == "variable") {
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(abind, c(dots, along = 3L))
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(abind, c(dots, along = 2L))
  } else if (along == "iteration") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, chain_ids)
    out <- do.call(abind, c(dots, along = 1L))
  } else if (along == "draw") {
    stop_no_call("Cannot bind 'draws_array' objects along 'draw'.")
  }
  as_draws_array(out)
}

#' @rdname bind_draws
#' @export
bind_draws.draws_df <- function(x, ..., along = "variable") {
  along <- validate_along(along)
  dots <- list(...)
  if (!length(dots)) {
    return(as_draws_df(x))
  }
  dots <- c(list(x), dots)
  dots <- remove_null(dots)
  dots <- lapply(dots, as_draws_df)
  dots <- lapply(dots, repair_draws)
  if (along == "variable") {
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    reserved_df_values <- as.data.frame(dots[[1]])[, c(".chain", ".iteration")]
    dots <- lapply(dots, remove_reserved_df_variables)
    out <- do.call(cbind, dots)
    out <- cbind(out, reserved_df_values)
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    cumsum_chains <- c(0, cumsum(ulapply(dots, nchains)))
    for (i in seq_along(dots)) {
      dots[[i]]$.chain <- cumsum_chains[i] + dots[[i]]$.chain
      dots[[i]]$.chain <- as.integer(dots[[i]]$.chain)
    }
    out <- do.call(rbind, dots)
  } else if (along == "iteration") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, chain_ids)
    cumsum_iterations <- c(0, cumsum(ulapply(dots, niterations)))
    for (i in seq_along(dots)) {
      dots[[i]]$.iteration <- cumsum_iterations[i] + dots[[i]]$.iteration
      dots[[i]]$.iteration <- as.integer(dots[[i]]$.iteration)
    }
    out <- do.call(rbind, dots)
  } else if (along == "draw") {
    check_same_fun_output(dots, variables)
    out <- do.call(rbind, dots)
    # binding along 'draw' implies dropping chain information
    out$.chain <- 1L
    out$.iteration <- seq_rows(out)
  }
  out$.draw <- NULL
  .as_draws_df(out)
}

#' @rdname bind_draws
#' @export
bind_draws.draws_list <- function(x, ..., along = "variable") {
  along <- validate_along(along)
  dots <- list(...)
  if (!length(dots)) {
    return(as_draws_list(x))
  }
  dots <- c(list(x), dots)
  dots <- remove_null(dots)
  dots <- lapply(dots, as_draws_list)
  dots <- lapply(dots, repair_draws)
  if (along == "variable") {
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    chains <- seq_along(dots[[1]])
    out <- vector("list", length(chains))
    for (i in chains) {
      out[[i]] <- ulapply(dots, "[[", i, recursive = FALSE)
    }
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(c, dots)
  } else if (along == "iteration") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, chain_ids)
    chains <- seq_along(dots[[1]])
    variables <- variables(dots[[1]])
    out <- vector("list", length(chains))
    for (i in chains) {
      tmp <- lapply(dots, "[[", i)
      out[[i]] <- named_list(variables)
      for (v in variables) {
        out[[i]][[v]] <- ulapply(tmp, "[[", v)
      }
    }
  } else if (along == "draw") {
    stop_no_call("Cannot bind 'draws_list' objects along 'draw'.")
  }
  as_draws_list(out)
}

#' @rdname bind_draws
#' @importFrom abind abind
#' @export
bind_draws.draws_rvars <- function(x, ..., along = "variable") {
  along <- validate_along(along)
  dots <- list(...)
  if (!length(dots)) {
    return(as_draws_rvars(x))
  }
  dots <- c(list(x), dots)
  dots <- remove_null(dots)
  dots <- lapply(dots, as_draws_rvars)
  dots <- lapply(dots, repair_draws)
  if (along == "variable") {
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(c, dots)
  } else if (along == "iteration") {
    stop_no_call("Cannot bind 'draws_rvars' objects along 'iteration'.")
  } else if (along %in% c("chain", "draw")) {
    check_same_fun_output(dots, variables)
    if (along == "chain") {
      check_same_fun_output(dots, iteration_ids)
      nchains <- sum(sapply(dots, nchains))
    } else {
      # binding along 'draw' implies dropping chain information
      dots <- lapply(dots, merge_chains)
      nchains <- 1
    }
    # bind all the corresponding variables together along draws
    out <- lapply(seq_along(dots[[1]]), function(var_i) {
      vars <- lapply(dots, `[[`, var_i)
      var_draws <- lapply(vars, draws_of)
      out <- rvar(abind(var_draws, along = 1), nchains = nchains)
      out
    })
    names(out) <- names(dots[[1]])
  }
  as_draws_rvars(out)
}

#' @export
bind_draws.NULL <- function(x, ..., along = "variable") {
  dots <- list(...)
  dots <- remove_null(dots)
  if (!length(dots)) {
    stop_no_call("All objects passed to 'bind_draws' are NULL.")
  }
  do.call(bind_draws, dots)
}

# check if function output is the same across objects
# @param ls list of objects
# @param fun a function to be evaluated on the objects
# @param TRUE if the function output matches for all objects
check_same_fun_output <- function(ls, fun) {
  assert_list(ls)
  if (is.function(fun)) {
    fun_name <- deparse_pretty(substitute(fun))
  } else {
    fun_name <- as_one_character(fun)
    fun <- get(fun, mode = "function")
  }
  ids <- lapply(ls, fun)
  if (sum(!duplicated(ids)) > 1L) {
    stop_no_call("'", fun_name, "' of bound objects do not match.")
  }
  invisible(TRUE)
}

# validate the 'along' argument of 'bind_draws'
validate_along <- function(along) {
  options <- c("variable", "chain", "iteration", "draw")
  match.arg(along, options)
}

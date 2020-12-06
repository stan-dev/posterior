#' Bind `draws` objects together
#'
#' Bind multiple [`draws`] objects together to form a single
#' `draws` object.
#'
#' @param x A [`draws`] object. The draws format of `x` will define the format
#'   of the returned draws object.
#' @param ... Further [`draws`] objects.
#' @param along The dimenion along with draws objects should be bind together.
#'   Possible values are `variable` (the default), `chain`, `iteration`, and
#'   `draw`. Not all options are supported for all input formats.
#' @template return-draws
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
    check_same_fun_output(dots, draw_ids)
    out <- do_call(abind, c(dots, along = 2L))
  } else if (along %in% c("iteration", "draw")) {
    check_same_fun_output(dots, variables)
    out <- do_call(abind, c(dots, along = 1L))
  } else if (along == "chain") {
    stop2("Cannot bind 'draws_matrix' objects along 'chain'.")
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
    out <- do_call(abind, c(dots, along = 3L))
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    out <- do_call(abind, c(dots, along = 2L))
  } else if (along == "iteration") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, chain_ids)
    out <- do_call(abind, c(dots, along = 1L))
  } else if (along == "draw") {
    stop2("Cannot bind 'draws_array' objects along 'draw'.")
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
    reserved_df_values <- dots[[1]][, c(".chain", ".iteration")]
    dots <- lapply(dots, remove_reserved_df_variables)
    out <- do_call(cbind, dots)
    out <- cbind(out, reserved_df_values)
  } else if (along == "chain") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, iteration_ids)
    nchains <- ulapply(dots, nchains)
    for (i in seq_along(dots)) {
      dots[[i]]$.chain <- sum(nchains[i - 1]) + dots[[i]]$.chain
      dots[[i]]$.chain <- as.integer(dots[[i]]$.chain)
    }
    out <- do_call(rbind, dots)
  } else if (along == "iteration") {
    check_same_fun_output(dots, variables)
    check_same_fun_output(dots, chain_ids)
    niterations <- ulapply(dots, niterations)
    for (i in seq_along(dots)) {
      dots[[i]]$.iteration <- sum(niterations[i - 1]) + dots[[i]]$.iteration
      dots[[i]]$.iteration <- as.integer(dots[[i]]$.iteration)
    }
    out <- do_call(rbind, dots)
  } else if (along == "draw") {
    check_same_fun_output(dots, variables)
    out <- do_call(rbind, dots)
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
    out <- do_call(c, dots)
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
    stop2("Cannot bind 'draws_list' objects along 'draw'.")
  }
  as_draws_list(out)
}

#' @rdname bind_draws
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
    # TODO: depending on resolution of #81, update check here
    check_same_fun_output(dots, chain_ids)
    check_same_fun_output(dots, iteration_ids)
    out <- do.call(c, dots)
  } else if (along == "chain") {
    stop2("TODO: implement")
  } else if (along == "iteration") {
    stop2("TODO: implement")
  } else if (along == "draw") {
    stop2("TODO: implement")
  }
  as_draws_rvars(out)
}

#' @export
bind_draws.NULL <- function(x, ..., along = "variable") {
  dots <- list(...)
  dots <- remove_null(dots)
  if (!length(dots)) {
    stop2("All objects passed to 'bind_draws' are NULL.")
  }
  do_call(bind_draws, dots)
}

# check if function output is the same across objects
# @param ls list of objects
# @param fun a function to be evaluated on the objects
# @param TRUE if the function output matches for all objects
check_same_fun_output <- function(ls, fun) {
  assert_list(ls)
  if (is.function(fun)) {
    fun_name <- deparse2(substitute(fun))
  } else {
    fun_name <- as_one_character(fun)
    fun <- get(fun, mode = "function")
  }
  ids <- lapply(ls, fun)
  if (sum(!duplicated(ids)) > 1L) {
    stop2("'", fun_name, "' of bound objects do not match.")
  }
  invisible(TRUE)
}

# validate the 'along' argument of 'bind_draws'
validate_along <- function(along) {
  options <- c("variable", "chain", "iteration", "draw")
  match.arg(along, options)
}

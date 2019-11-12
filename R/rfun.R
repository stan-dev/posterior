#' Create functions of random variables
#'
#' Function that create functions that can accept and/or produce random variables.
#'
#' @param .f A function (or a one-sided formula representing a function that can be parsed by
#' [rlang::as_function()]) to turn into a function that accepts and/or produces random variables.
#' @param rvar_args The arguments of `.f` that should be allowed to accept [rvar]s as arguments.
#' If `NULL` (the default), all arguments to `.f` are turned into arguments that accept
#' [rvar]s.
#' @param ndraws When no [rvar]s are supplied as arguments to the new function, this is the number
#' of draws that will be used to construct new random variables. If `NULL`,
#' getOption("rvar.ndraws") is used (default 4000).
#'
#' @details This function wraps an existing funtion (`.f`) such that it returns [rvar]s containing
#' whatever type of data `.f` would normally return.
#'
#' @return A function with the same argument specification as `.f`, but which can accept and return
#' [rvar]s.
#'
#' @export
rfun <- function (.f, rvar_args = NULL, ndraws = NULL) {
  # based loosely on base::Vectorize
  ndraws <- ndraws %||% getOption("rvar.ndraws", 4000)
  .f <- rlang::as_function(.f)

  arg_names <- as.list(formals(.f))
  arg_names[["..."]] <- NULL
  arg_names <- names(arg_names)
  rvar_args <- as.character(rvar_args %||% arg_names)

  if (!all(rvar_args %in% arg_names)) {
    stop("must specify names of formal arguments for 'rfun'")
  }

  collisions <- arg_names %in% c("FUN", "SIMPLIFY", "USE.NAMES")
  if (any(collisions)) {
    stop2("'.f' may not have argument(s) named ", comma(arg_names[collisions]))
  }

  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    arg_names <-
      if (is.null(names(args))) character(length(args))
      else names(args)

    is_rvar_arg <- (arg_names %in% rvar_args) & as.logical(lapply(args, is_rvar))
    rvar_args <- lapply(args[is_rvar_arg], list_of_draws)

    if (length(rvar_args) == 0) {
      # no rvar arguments, so just create a random variable by applying this function
      # ndraws times
      list_of_draws <- replicate(ndraws, do.call(.f, args), simplify = FALSE)
    } else {
      list_of_draws <- do.call(mapply, c(FUN = .f, rvar_args, MoreArgs = list(args[!is_rvar_arg]),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      ))
    }
    # TODO: could speed this up if we assert all vectors must have same dimensions, then
    # it's a straigtforward concatenation + setting the dimensions to be the c(dim(vec[[1]]), length(vecs))
    new_rvar(abind::abind(list_of_draws, rev.along = 0))
  }
  formals(FUNV) <- formals(.f)
  FUNV
}

rvar_r <- function(.f, n, ..., ndraws = 4000) {
  ndraws <- ndraws %||% getOption("rvar.ndraws", 4000)
  args = list(...)
  is_rvar_arg <- as.logical(lapply(args, is_rvar))
  rvar_args = args[is_rvar_arg]

  rvar_args_ndraws = sapply(rvar_args, ndraws)
  if (length(rvar_args_ndraws) != 0) {
    ndraws = max(rvar_args_ndraws)
    if (!all(rvar_args_ndraws == 1 | rvar_args_ndraws == ndraws)) {
      stop2("All arguments must have a compatible number of draws")
    }

    rvar_args_ndims = sapply(rvar_args, function(x) length(dim(x)))
    if (!all(rvar_args_ndims == 0)) {
      stop2("All arguments must be single-dimensional rvars")
    }

    args[is_rvar_arg] <- lapply(rvar_args, draws_of)
  }

  nd = n * ndraws
  args = c(list(n = nd), args)
  result = do.call(.f, args)
  dim(result) = c(n, ndraws)
  new_rvar(result)
}

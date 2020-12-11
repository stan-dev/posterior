#' Create functions of random variables
#'
#' Function that create functions that can accept and/or produce random variables.
#'
#' @param .f A function (or a one-sided formula representing a function that can be parsed by
#' [rlang::as_function()]) to turn into a function that accepts and/or produces random variables.
#' @param rvar_args The arguments of `.f` that should be allowed to accept [`rvar`]s as arguments.
#' If `NULL` (the default), all arguments to `.f` are turned into arguments that accept
#' [`rvar`]s.
#' @param ndraws When no [`rvar`]s are supplied as arguments to the new function, this is the number
#' of draws that will be used to construct new random variables. If `NULL`,
#' `getOption("posterior.rvar_ndraws")` is used (default 4000).
#'
#' @details This function wraps an existing funtion (`.f`) such that it returns [`rvar`]s containing
#' whatever type of data `.f` would normally return.
#'
#' @return A function with the same argument specification as `.f`, but which can accept and return
#' [`rvar`]s.
#'
#' @export
rfun <- function (.f, rvar_args = NULL, ndraws = NULL) {
  # based loosely on base::Vectorize
  ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000)
  .f <- rlang::as_function(.f)

  arg_names <- as.list(formals(.f))
  arg_names[["..."]] <- NULL
  arg_names <- names(arg_names)
  rvar_args <- as.character(rvar_args %||% arg_names)

  if (!all(rvar_args %in% arg_names)) {
    stop2("must specify names of formal arguments for 'rfun'")
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

    # convert arguments that are rvars into draws the function can be applied over
    is_rvar_arg <- (arg_names %in% rvar_args) & as.logical(lapply(args, is_rvar))
    rvar_args <- conform_rvar_ndraws_nchains(args[is_rvar_arg])
    .nchains <- if (length(rvar_args) < 1) {
      1
    } else {
      nchains(rvar_args[[1]])
    }
    rvar_args <- lapply(rvar_args, list_of_draws)

    if (length(rvar_args) == 0) {
      # no rvar arguments, so just create a random variable by applying this function
      # ndraws times
      list_of_draws <- replicate(ndraws, do.call(.f, args), simplify = FALSE)
    } else {
      list_of_draws <- do.call(mapply, c(FUN = .f, rvar_args, MoreArgs = list(args[!is_rvar_arg]),
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      ))
    }
    # Need to add a first dimension before unchopping (this will be the draws dimension)
    # Doing this + vec_unchop is faster than doing abind::abind(list_of_draws, along = 0)
    list_of_draws <- lapply(list_of_draws, function(x) {
      x <- as.array(x)
      dim(x) <- c(1, dim(x))
      x
    })
    new_rvar(vctrs::vec_unchop(list_of_draws), .nchains = .nchains)
  }
  formals(FUNV) <- formals(.f)
  FUNV
}

#' Execute expressions of random variables
#'
#' Execute an expression to create a random variable.
#'
#' @param expr A bare expression that can (optionally) contain [`rvar`]s. The expression
#' supports [quasiquotation].
#' @param ndraws When no [`rvar`]s are supplied in `expr`, this is the number
#' of draws that will be used to construct new random variables. If `NULL`,
#' `getOption("posterior.rvar_ndraws")` is used (default 4000).
#' @template args-rvar-dim
#'
#' @details This function evaluates `expr` possibly multiple times, once for each draw of
#' the [`rvar`]s it contains, then returns a new [`rvar`] representing the output of those
#' expressions. To identify [`rvar`]s, `rdo()` searches the calling environment for any variables
#' named in `expr` for which [is_rvar()] evaluates to `TRUE`. If `expr` contains no [`rvar`]s,
#' then it will be executed `ndraws` times and an [`rvar`] with that many draws returned.
#'
#' `rdo()` is not necessarily *fast* (in fact in some cases it may be very slow), but
#' it has the advantage of allowing nearly arbitrary R code to be executed against [`rvar`]s
#' simply by wrapping it with `rdo( ... )`. This makes it especially useful as a prototyping
#' tool. If you create code with `rdo()` and it is unacceptably slow for your application,
#' consider rewriting it in terms of math operations directly on [`rvar`]s (which should be fast),
#' or in terms of operations directly on the arrays that back the [`rvar`]s, using [draws_of()].
#'
#' @return An [`rvar`].
#'
#' @examples
#'
#' mu <- rdo(rnorm(1, 0, 1))
#' sigma <- rdo(rgamma(1, 1, 1))
#' x <- rdo(rnorm(10, mu, sigma))
#' x
#'
#' @importFrom rlang eval_tidy quo_get_env enquo missing_arg quo_get_expr
#' @export
rdo <- function(expr, dim = NULL, ndraws = NULL) {
  ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000)

  # basic idea here is to find all the variables that are used in the expression
  # and which are also random variables in the expression's environment, then
  # build a function that executes the expression and takes those random
  # variables as arguments, then vectorize that function using `rfun()` and execute it.
  f_expr <- enquo(expr)
  f_env <- quo_get_env(f_expr)

  rvars_in_expr <- list()
  f_alist <- alist()
  for (name in all.vars(f_expr)) {
    var <- get(name, f_env)
    if (is_rvar(var)) {
      rvars_in_expr[[name]] <- var
      f_alist[[name]] <- missing_arg()
    }
  }

  f_alist <- append(f_alist, quo_get_expr(f_expr))
  f <- rfun(as.function(f_alist, envir = f_env), ndraws = ndraws)
  result <- do.call(f, rvars_in_expr, envir = f_env)

  if (!is.null(dim)) {
    dim(result) <- dim
  }
  result
}

rvar_r <- function(.f, n, ..., ndraws = NULL) {
  # TODO: finish and test this, maybe export?
  ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000)
  args = list(...)
  is_rvar_arg <- as.logical(lapply(args, is_rvar))
  rvar_args = conform_rvar_ndraws_nchains(args[is_rvar_arg])
  .nchains <- if (length(rvar_args) < 1) {
    1
  } else {
    nchains(rvar_args[[1]])
  }

  rvar_args_ndraws = sapply(rvar_args, ndraws)
  if (length(rvar_args_ndraws) != 0) {

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
  new_rvar(t(result), .nchains = .nchains)
}

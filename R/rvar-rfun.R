#' Create functions of random variables
#'
#' Function that create functions that can accept and/or produce [`rvar`]s.
#'
#' @param .f (multiple options) A function to turn into a function that accepts
#'   and/or produces random variables:
#'   * A function
#'   * A one-sided formula that can be parsed by [rlang::as_function()]
#' @param rvar_args (character vector) The names of the arguments of `.f` that
#'   should be allowed to accept [`rvar`]s as arguments. If `NULL` (the
#'   default), all arguments to `.f` are turned into arguments that accept
#'   [`rvar`]s.
#' @param ndraws (positive integer). The number of draws used to construct new
#'   random variables if no [`rvar`]s are supplied as arguments to the
#'   returned function. If `NULL`, `getOption("posterior.rvar_ndraws")` is used
#'   (default `4000`). If any arguments to the returned function contain
#'   [`rvar`]s, the number of draws in the provided [`rvar`]s is used instead of
#'   the value of this argument.
#'
#' @details This function wraps an existing function (`.f`) such that it returns [`rvar`]s containing
#' whatever type of data `.f` would normally return.
#'
#' The returned function, when called, executes `.f` possibly multiple times, once for each draw of
#' the [`rvar`]s passed to it, then returns a new
#' [`rvar`] representing the output of those function evaluations. If the arguments contain no [`rvar`]s,
#' then `.f` will be executed `ndraws` times and an [`rvar`] with that many draws returned.
#'
#' Functions created by `rfun()` are not necessarily *fast* (in fact in some cases they may be very slow), but
#' they have the advantage of allowing a nearly arbitrary R functions to be executed against [`rvar`]s
#' simply by wrapping them with `rfun()`. This makes it especially useful as a prototyping
#' tool. If you create code with `rfun()` and it is unacceptably slow for your application,
#' consider rewriting it using math operations directly on [`rvar`]s (which should be fast),
#' using [rvar_rng()], and/or using operations directly on the arrays that back the [`rvar`]s
#' (via [draws_of()]).
#'
#'
#' @return A function with the same argument specification as `.f`, but which can accept and return
#' [`rvar`]s.
#'
#' @examples
#'
#' rvar_norm <- rfun(rnorm)
#' rvar_gamma <- rfun(rgamma)
#'
#' mu <- rvar_norm(10, mean = 1:10, sd = 1)
#' sigma <- rvar_gamma(1, shape = 1, rate = 1)
#' x <- rvar_norm(10, mu, sigma)
#' x
#'
#' @family rfun
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
    stop_no_call("must specify names of formal arguments for 'rfun'")
  }

  collisions <- arg_names %in% c("FUN", "SIMPLIFY", "USE.NAMES")
  if (any(collisions)) {
    stop_no_call("'.f' may not have argument(s) named ", comma(arg_names[collisions]))
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
#' Execute (nearly) arbitrary \R expressions that may include [`rvar`]s,
#' producing a new [`rvar`].
#'
#' @param expr (expression) A bare expression that can (optionally) contain
#'   [`rvar`]s. The expression supports [quasiquotation].
#' @param ndraws (positive integer) The number of draws used to construct new
#'   random variables if no [`rvar`]s are supplied in `expr`. If `NULL`,
#'   `getOption("posterior.rvar_ndraws")` is used (default 4000). If `expr`
#'   contains [`rvar`]s, the number of draws in the provided [`rvar`]s is used
#'   instead of the value of this argument.
#' @template args-rvar-dim
#'
#' @details This function evaluates `expr` possibly multiple times, once for each draw of
#' the [`rvar`]s it contains, then returns a new [`rvar`] representing the output of those
#' expressions. To identify [`rvar`]s, `rdo()` searches the calling environment for any variables
#' named in `expr` for which [is_rvar()] evaluates to `TRUE`. If `expr` contains no [`rvar`]s,
#' then it will be executed `ndraws` times and an [`rvar`] with that many draws returned.
#'
#' `rdo()` is not necessarily *fast* (in fact in some cases it may be very slow), but
#' it has the advantage of allowing a nearly arbitrary R expression to be executed against [`rvar`]s
#' simply by wrapping it with `rdo( ... )`. This makes it especially useful as a prototyping
#' tool. If you create code with `rdo()` and it is unacceptably slow for your application,
#' consider rewriting it using math operations directly on [`rvar`]s (which should be fast),
#' using [rvar_rng()], and/or using operations directly on the arrays that back the [`rvar`]s
#' (via [draws_of()]).
#'
#' @return An [`rvar`].
#'
#' @examples
#'
#' mu <- rdo(rnorm(10, mean = 1:10, sd = 1))
#' sigma <- rdo(rgamma(1, shape = 1, rate = 1))
#' x <- rdo(rnorm(10, mu, sigma))
#' x
#'
#' @family rfun
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

#' Create random variables from existing random number generators
#'
#' Specialized alternative to `rdo()` or `rfun()` for creating [`rvar`]s from
#' existing random-number generator functions (such as `rnorm()`, `rbinom()`, etc).
#'
#' @param .f (function) A function (or string naming a function) representing a
#'   random-number generating function that follows the pattern of base random
#'   number generators (like `rnorm()`, `rbinom()`, etc). It must:
#'   - Have a first argument, `n`, giving the number of draws to take from the
#'     distribution
#'   - Have vectorized parameter arguments
#'   - Return a single vector of length `n`
#' @param n (positive integer) The length of the output [`rvar`] vector (**not**
#'   the number of draws).
#' @param ... Arguments passed to `.f`. These arguments may include [`rvar`]s,
#'   so long as they are vectors only (no multidimensional [`rvar`]s are
#'   allowed).
#' @param ndraws (positive integer) The number of draws used to construct the
#'   returned random variable if no [`rvar`]s are supplied in `...`. If `NULL`,
#'   `getOption("posterior.rvar_ndraws")` is used (default 4000). If `...`
#'   contains [`rvar`]s, the number of draws in the provided [`rvar`]s is used
#'   instead of the value of this argument.
#'
#' @details This function unwraps the arrays underlying the input [`rvar`]s in
#' `...` and then passes them to `.f`, relying on the vectorization of `.f`
#' to evaluate it across draws from the input [`rvar`]s. This is why the arguments
#' of `.f` **must** be vectorized. It asks for `n` times the number of draws
#' in the input [`rvar`]s (or `ndraws` if none are given) draws from the
#' random number generator `.f`, then reshapes the output from `.f` into an
#' [`rvar`] with length `n`.
#'
#' `rvar_rng()` is a fast alternative to `rdo()` or `rfun()`, but you **must**
#' ensure that `.f` satisfies the preconditions described above for the result
#' to be correct. Most base random number generators satisfy these conditions.
#' It is advisable to test against `rdo()` or `rfun()` (which should be correct,
#' but slower) if you are uncertain.
#'
#' @return A single-dimensional [`rvar`] of length `n`.
#'
#' @examples
#'
#' mu <- rvar_rng(rnorm, 10, mean = 1:10, sd = 1)
#' sigma <- rvar_rng(rgamma, 1, shape = 1, rate = 1)
#' x <- rvar_rng(rnorm, 10, mu, sigma)
#' x
#'
#' @family rfun
#' @export
rvar_rng <- function(.f, n, ..., ndraws = NULL) {
  args <- list(...)

  is_rvar_arg <- vapply(args, is_rvar, logical(1))
  rvar_args <- conform_rvar_ndraws_nchains(args[is_rvar_arg])

  if (length(rvar_args) < 1) {
    nchains <- 1
    ndraws <- ndraws %||% getOption("posterior.rvar_ndraws", 4000)
  } else {
    # we have some arguments that are rvars. We require them to be single-dimensional
    # (vectors) so that R's vector recycling will work correctly.
    nchains <- nchains(rvar_args[[1]])
    ndraws <- ndraws(rvar_args[[1]])

    rvar_args_ndims <- lengths(lapply(rvar_args, dim))
    if (!all(rvar_args_ndims == 1)) {
      stop_no_call("All arguments to rvar_rng() that are rvars must be single-dimensional (vectors).")
    }

    args[is_rvar_arg] <- lapply(rvar_args, function(x) as.vector(draws_of(x)))
  }

  # we must manually recycle numeric vector arguments up to the desired number
  # of draws so that they can be correctly recycled along the draws of any input
  # rvars. We only convert numeric *vectors*, as (1) scalars can be recycled
  # as-is and (2) matrices and 2d+ arrays cannot be correctly recycled using R's
  # recycling rules so they are typically only used as constant arguments to
  # random number generator functions (e.g. Sigma for a multivariate normal),
  # so we don't need to worry about them.
  is_numeric_vector_arg <-
    vapply(args, function(x) is.numeric(x) && length(x) > 1 && length(dim(x)) <= 1, logical(1)) &
    !is_rvar_arg
  args[is_numeric_vector_arg] <- lapply(args[is_numeric_vector_arg], rep, each = ndraws)

  nd <- n * ndraws
  args <- c(n = nd, args)
  result <- do.call(.f, args)
  dim(result) <- c(ndraws, n)
  new_rvar(result, .nchains = nchains)
}
